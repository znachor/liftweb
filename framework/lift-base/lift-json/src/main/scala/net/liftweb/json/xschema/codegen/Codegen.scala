package net.liftweb.json.xschema.codegen

import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.Validation._
import _root_.net.liftweb.json.Printer._
import _root_.net.liftweb.json.xschema.XSchemaAST._
import _root_.net.liftweb.json.xschema.XSchemaDatabase

import java.io.{FileOutputStream, Writer}

import scala.collection.mutable.{Map => MutableMap}

class State(var indentLevel: Int) {
  def indent   = { indentLevel = indentLevel + 1; this }
  def unindent = { indentLevel = indentLevel - 1; this }
  
  def replaceWith(that: State) = { this.indentLevel = that.indentLevel; this }
  
  def tab = "  "
  
  def startIndentation = (0 to indentLevel).foldLeft("") { (cur, l) => cur + tab }
  
  def column = tab.length * indentLevel
}

object State {
  def apply(indentLevel: Int) = new State(indentLevel)
}

case class CodeBuilder(codeBuilder: StringBuilder, state: State) {
  import scala.collection.mutable.ArrayStack
  
  private val replacements = new ArrayStack[List[(String, String)]]
  
  def += (str: String): CodeBuilder = { 
    var newStr = replacements.toList.foldRight(str) { (replacement, str) => replace(str, replacement) }
    
    var isFirst = true
    
    newStr.split("\n").foreach { s =>
      if (isFirst) isFirst = false else newline
      
      codeBuilder.append(s)
    }
    
    this
  }
  
  def += (that: CodeBuilder): CodeBuilder = { 
    this += that.code
    
    this.state.replaceWith(that.state)
    this
  }
  
  def addln(template: String, replacements: (String, String)*) = add(template, replacements: _*).newline
  
  def add(template: String, replacements: (String, String)*): CodeBuilder = { this += replace(template, replacements) }
  
  def using[T](replacements: (String, String)*)(f: => T): T = {
    this.replacements.push(replacements.toList)
    
    val returnValue = f
    
    this.replacements.pop
    
    returnValue
  }
  
  def indent   = { state.indent; newline }
  def unindent = { state.unindent; newline }
  def newline  = { codeBuilder.append("\n").append(state.startIndentation); this }
  
  def newline(n: Int): CodeBuilder = { (0 until n) foreach { x => newline }; this }
  
  def join[T](iterable: Iterable[T], joiner: => Unit)(f: T => Unit): Unit = {
    var isFirst = true
    
    for (element <- iterable) {
      if (isFirst) isFirst = false
      else joiner
      
      f(element)
    }
  }
  
  def code = codeBuilder.toString
  
  private def replace(template: String, replacements: Iterable[(String, String)]) = replacements.foldLeft(template) { (t, r) => t.replace("${" + r._1 + "}", r._2) }
}

object CodeBuilder {
  def empty = new CodeBuilder(new StringBuilder(), State(0))
}

case class CodeBundle(fileToCG: MutableMap[String, CodeBuilder]) {
  def += (tuple: (String, CodeBuilder)) = {
    val file  = tuple._1
    val oldCG = forFile(file)
    val newCG = tuple._2
    
    fileToCG += file -> (oldCG += newCG)
  }
  
  def create(root: String)(implicit writerF: String => Writer) = {
    for ((file, cg) <- fileToCG) {
      val absPath = root + "/" + file
      
      val os = writerF(absPath)
      
      try {
        os.write(cg.code)
        os.flush
      }
      finally {
        os.close();
      }
    }
  }
  
  private def forFile(file: String) = if (fileToCG.contains(file)) fileToCG(file) else {
    fileToCG += file -> CodeBuilder.empty
    
    fileToCG(file)
  }
}

object CodeBundle {
  def empty = new CodeBundle(MutableMap())
}

trait CodeGeneratorHelpers {
  def toDirectory(ns: Namespace) = ns.value.replace(".", "/") + "/"

  def toFile(ns: Namespace, name: String, extension: String): String = toDirectory(ns) + name + "." + extension
}

trait CodeGenerator {
  def generate(root: XRoot, destPath: String)(implicit writerF: String => Writer)
}


object ScalaCodeGenerator extends CodeGenerator with CodeGeneratorHelpers {
  def generate(root: XRoot, destPath: String)(implicit writerF: String => Writer) = {
    val bundle   = CodeBundle.empty
    val database = XSchemaDatabase(root.definitions)
    
    for (namespace <- database.namespaces) {
      val dataFile = toFile(namespace, "Data", "scala")
      
      val code = CodeBuilder.empty
      
      code.newline.add("package " + namespace.value + " {").indent
      
      code.addln("import net.liftweb.json.{SerializationImplicits, DefaultExtractors, ExtractionHelpers, DefaultDecomposers, DecomposerHelpers, DefaultOrderings}")
      code.addln("import net.liftweb.json.JsonParser._")
      code.addln("import net.liftweb.json.JsonAST._")
      code.addln("import net.liftweb.json.XSchema._")
      
      for (definition <- database.definitionsIn(namespace)) {    
        code.newline
        
        buildDataFor(definition, code, database)
      }
      
      buildExtractorsFor(namespace, code, database)
      buildDecomposersFor(namespace, code, database)
      
      buildPackageObjectFor(namespace, code, database)
      
      code.unindent.add("}")
      
      bundle += dataFile -> code
    }
    
    bundle.create(destPath)
  }
  
  private def buildDataFor(definition: XDefinition, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    walk(definition, code, definitionWalker(database)).newline
  }
  
  private def buildExtractorsFor(namespace: Namespace, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    code.newline(2).add("trait Extractors extends DefaultExtractors with ExtractionHelpers {").indent
    
    for (definition <- database.definitionsIn(namespace)) {
      definition match {
        case x: XProduct => 
          code.using("type" -> x.name) {
            code.add("implicit val ${type}Extractor: Extractor[${type}] = new Extractor[${type}] {").indent
          
            code.add("def extract(jvalue: JValue): ${type} = {").indent
            code.add(x.name).add("(").indent
          
            var isFirst = true
          
            code.join(x.fields, code.add(",").newline) { field =>
              code.add("extractField[${fieldType}](jvalue, \"${fieldName}\", \"\"\"" + compact(render(field.defValue)) + " \"\"\")", 
                "fieldType" -> field.fieldType.typename,
                "fieldName" -> field.name
              )
            }
          
            code.unindent.add(")").unindent.add("}").unindent.add("}").newline
          }
          
        case x: XCoproduct =>
          code.using("type" -> x.name) {
            code.add("private lazy val ${type}ExtractorFunction: PartialFunction[JField, ${type}] = List[PartialFunction[JField, ${type}]](").indent
            
            code.join(x.types, code.add(",").newline) { typ =>
              code.add("{ case JField(\"${typeName}\", value) => ${typeName}Extractor.extract(value) }",
                "typeName" -> typ.name
              )
            }
            
            code.unindent.add(").reduceLeft { (a, b) => a.orElse(b) }").newline.newline
            
            code.add("implicit val ${type}Extractor: Extractor[${type}] = new Extractor[${type}] {").indent
            code.add("def extract(jvalue: JValue): ${type} = {").indent
            code.add("(jvalue --> classOf[JObject]).obj.filter(${type}ExtractorFunction.isDefinedAt _) match {").indent
            code.add("case field :: fields => ${type}ExtractorFunction(field)").newline.newline
            code.add("case Nil => error(\"Expected to find ${type} but found \" + jvalue)")
            
            code.unindent.add("}").unindent.add("}").unindent.add("}").newline
            
            
          }
      }
    }
    
    code.unindent.add("}").newline
  }
  
  private def buildDecomposersFor(namespace: Namespace, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    code.newline(2).add("trait Decomposers extends DefaultDecomposers with DecomposerHelpers {").indent
    
    for (definition <- database.definitionsIn(namespace)) {
      definition match {
        case x: XProduct => 
          code.using("type" -> x.name) {
            code.add("implicit val ${type}Decomposer: Decomposer[${type}] = new Decomposer[${type}] {").indent
          
            code.add("def decompose(tvalue: ${type}): JValue = {").indent
            code.add("JObject(").indent
          
            var isFirst = true
          
            x.fields foreach { field =>
              code.add("JField(\"${fieldType}\", tvalue.${fieldType}.serialize) ::", "fieldType" -> field.name).newline
            }
          
            code.add("Nil")
          
            code.unindent.add(")").unindent.add("}").unindent.add("}").newline
          }
        
        case x: XCoproduct =>
          code.using("type" -> x.name) {
            code.add("implicit val ${type}Decomposer: Decomposer[${type}] = new Decomposer[${type}] {").indent
            
            code.add("def decompose(tvalue: ${type}): JValue = {").indent
            
            code.add("tvalue match {").indent
            
            x.types foreach { typ =>
              code.addln("case x: ${productType} => JObject(JField(\"${productType}\", decompose(x)) :: Nil)",
                "productType" -> typ.name
              )
            }
            
            code.unindent.add("}").unindent.add("}").unindent.add("}").newline
          }
      }
    }
    
    code.unindent.add("}")
  }
  
  private def buildPackageObjectFor(namespace: Namespace, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    code.newline(2).add("object Serialization extends SerializationImplicits with Decomposers with Extractors { }")
  }
  
  private def typeSignatureOf(x: XSchema): String = walk(x, CodeBuilder.empty, typeSignatureWalker).code
  
  private def definitionWalker(database: XSchemaDatabase) = new XSchemaDefinitionWalker[CodeBuilder] {
    override def begin(code: CodeBuilder, defn: XDefinition) = {
      def coproductPrefix(x: XCoproduct): String = if (database.productChildrenOf(x).map(_.namespace).removeDuplicates.length <= 1) "sealed " else ""
      def buildProductFields(x: XProduct): CodeBuilder = code.add(x.fields.map(typeSignatureOf(_)).mkString(", "))
      def buildCoproductFields(x: XCoproduct): CodeBuilder = {
        val commonFields = database.commonFieldsOf(x)
        
        code.join(commonFields, code.newline) { field =>
          code += ("def " + field._1 + ": " + field._2.typename)
        }
        
        code
      }
      
      defn match {
        case x: XProduct =>
          code.add("case class " + defn.name + "(")
          buildProductFields(x)
          code.add(")")
          
          val withClauses = ("Ordered[" + defn.name + "]" :: database.coproductContainersOf(x).map(_.qualifiedName)).mkString(" with ")
          
          code.add(" extends " + withClauses + " {").indent
          
        case x: XCoproduct => 
          code.add(coproductPrefix(x) + "trait " + x.name + " {").indent
          buildCoproductFields(x)
      }
    }
    
    override def end(code: CodeBuilder, defn: XDefinition) = {
      def buildOrderedDefinition(x: XProduct): CodeBuilder = {
        def buildComparisonFor(field: XField, schema: XSchema): CodeBuilder = {
          def comparisonSign = field.order match {
            case Ascending  => 1
            case Descending => -1
            case Ignore     => 0
          }
          
          def buildStandardComparison(): CodeBuilder = {
            code.addln("c = this." + field.name + ".compare(that." + field.name + ")")
            code.addln("if (c != 0) return c * " + comparisonSign.toString)
          }
                                   
          schema match {
            case x: XOptional   => buildComparisonFor(field, x.optionalType)
            case x: XCollection => code
            case x: XConstant   => buildComparisonFor(field, x.constantType)
            case x: XMap        => code
            case x: XTuple      => buildStandardComparison()
            
            case x: XPrimitive  => buildStandardComparison()
            case x: XReference  => buildStandardComparison()
            
            case x: XProduct    => error("Found definition in field")
            case x: XCoproduct  => error("Found definition in field")
            case x: XRoot       => error("Found root in field")
            case x: XField      => error("Found field in field")
          }
        }
        
        code.add("def compare(that: " + x.name + "): Int = {").indent.addln("if (this == that) return 0").newline.addln("var c: Int = 0").newline
        
        x.fields.foreach { field =>
          buildComparisonFor(field, field.fieldType)
          
          code.newline
        }
        
        code.add("return 0").unindent.add("}")
      }
      
      defn match {
        case x: XProduct => {
          buildOrderedDefinition(x)
        }
        case x: XCoproduct => 
      }
      
      code.unindent.add("}") // Close definition
    }
  }
  
  private lazy val typeSignatureWalker = new XSchemaDefinitionWalker[CodeBuilder] {
    override def begin(data: CodeBuilder, field: XField) = {
      data += field.name + ": "
    }
    
    override def begin(data: CodeBuilder, opt: XOptional) = {
      data += "Option["
    }
    
    override def begin(data: CodeBuilder, col: XCollection) = {
      data += ((col.collection match {
        case XSet   => "Set"
        case XArray => "Array"
        case XList  => "List"
      }) + "[")
    }
    
    override def begin(data: CodeBuilder, map: XMap) = {
      data += "Map[String, "
    }
    
    override def begin(data: CodeBuilder, tuple: XTuple) = {
      data += "(" + tuple.types.map(typeSignatureOf(_)).mkString(", ") + ")"
    }
    
    override def walk(data: CodeBuilder, prim: XPrimitive) = {
      data += (prim match {
        case XString  => "String"
        case XInt     => "Int"
        case XLong    => "Long"
        case XFloat   => "Float"
        case XDouble  => "Double"
        case XBoolean => "Boolean"
      })
    }
    
    override def walk(data: CodeBuilder, ref: XReference) = {
      data += ref.typename
    }
    
    override def end(data: CodeBuilder, opt: XOptional) = {
      data += "]"
    }
    
    override def end(data: CodeBuilder, col: XCollection) = {
      data += "]"
    }
    
    override def end(data: CodeBuilder, map: XMap) = {
      data += "]"
    }
  }
}
