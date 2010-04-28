package net.liftweb.json.xschema.codegen

import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.Validation._
import _root_.net.liftweb.json.Printer._
import _root_.net.liftweb.json.xschema.XSchemaAST._
import _root_.net.liftweb.json.xschema.Serialization._

import java.lang.StringBuilder
import java.io.{FileOutputStream, Writer}

import scala.collection.mutable.{Map => MutableMap}

class State(var indentLevel: Int) {
  def indent   = { indentLevel = indentLevel + 1; this }
  def unindent = { indentLevel = indentLevel - 1; this }
  
  def replaceWith(that: State) = { this.indentLevel = that.indentLevel; this }
  
  def tab = "  "
  
  def startIndentation = (0 until indentLevel).foldLeft("") { (cur, l) => cur + tab }
  
  def column = tab.length * indentLevel
}

object State {
  def apply(indentLevel: Int) = new State(indentLevel)
}

case class CodeBuilder(codeBuilder: StringBuilder, state: State) {
  import scala.collection.mutable.ArrayStack
  import scala.util.matching.Regex
  
  private val Indented = """^( +)(.*)$""".r
  
  private val replacements = new ArrayStack[List[(String, String)]]
  
  private var row = 0
  private var col = 0
  
  def += (str: String): CodeBuilder = { 
    var newStr = replacements.toList.foldRight(str) { (replacement, str) => replace(str, replacement) }
    
    var isFirst = true
    
    var indents = new ArrayStack[Int]
    
    var startIndentLevel = state.indentLevel
    
    var lines = newStr.split("\n")
    
    lines.foreach { line =>
      var strippedLine = line match {
        case Indented(spaces, rest) if (lines.length > 1) =>
          val count = spaces.length
          
          if (indents.size == 0) {
            indents.push(count)
          }
          else {
            val lastCount = indents.peek
            
            if (count != lastCount) {
              if (count > lastCount) {
                state.indent
                
                indents.push(count)
              }
              else if (count < lastCount) {
                while (indents.size > 0 && indents.peek != count) {
                  indents.pop
                  
                  state.unindent
                }
              }
            }
          }

          rest
          
        case _ => line
      }
    
      if (isFirst) isFirst = false else newline
      
      append(strippedLine)
    }
    
    state.indentLevel = startIndentLevel
    
    this
  }
  
  def += (that: CodeBuilder): CodeBuilder = { 
    this.append(that.code)
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
  
  def indent(f: => Unit): CodeBuilder = {
    indent
    
    f
    
    unindent
  }
  
  def block(f: => Unit): CodeBuilder = block(f, "{", "}")
  
  def paren(f: => Unit): CodeBuilder = block(f, "(", ")")
  
  def block(f: => Unit, begin: String, end: String): CodeBuilder = {
    add(begin).indent
    
    f
    
    unindent.add(end)
  }
  
  def wrap(str: String, linePrefix: String, limit: Int): CodeBuilder = {
    val words = str.split(" +")
    
    var isFirst = true
    
    for (i <- 0 until words.length) {
      if (isFirst) isFirst = false else add(" ")
      
      add(words(i))
      
      var peekLength = if (i < words.length - 1) words(i + 1).length + 1 else 0
      
      if (col + peekLength > limit) {
        newline.add(linePrefix)
        
        isFirst = true
      }
    }
    
    this
  }
  
  def apply(f: => String): CodeBuilder = add(f)
  
  def indent   = { state.indent; newline }
  def unindent = { state.unindent; newline }
  def newline  = append("\n").append(state.startIndentation)
  
  def newline(n: Int): CodeBuilder = { (0 until n) foreach { x => newline }; this }
  
  def join[T](iterable: Iterable[T], joiner: => Unit)(f: T => Unit): CodeBuilder = {
    var isFirst = true
    
    for (element <- iterable) {
      if (isFirst) isFirst = false else joiner
      
      f(element)
    }
    
    this
  }
  
  def code = codeBuilder.toString
  
  private def replace(template: String, replacements: Iterable[(String, String)]) = replacements.foldLeft(template) { (t, r) => t.replace("${" + r._1 + "}", r._2) }
  
  private def append(str: String): CodeBuilder = {
    for (i <- 0 until str.length) {
      if (str.charAt(i) == '\n') {
        row += 1
        col = 0
      }
      else {
        col += 1
      }
    }
    
    codeBuilder.append(str)
    
    this
  }
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
      
      code.newline.add("package " + namespace.value + " ").block {
        code.addln("import net.liftweb.json.JsonParser._").
             addln("import net.liftweb.json.JsonAST._").
             addln("import net.liftweb.json.xschema.{SerializationImplicits, DefaultExtractors, ExtractionHelpers, DefaultDecomposers, DecomposerHelpers, DefaultOrderings}").
             addln("import net.liftweb.json.xschema.Serialization._").
             addln("import net.liftweb.json.xschema.XSchemaAST")
        
        root.properties.get("scala.imports") match {
          case None => 
          case Some(imports) => imports.split(",(?![^{]+[}])").map("import " + _.trim) foreach { x => code.addln(x) }
        }
        
        code.newline
      
        code.join(database.coproductsIn(namespace) ++ database.productsIn(namespace), code.newline.newline) { definition =>
          buildDataFor(definition, code, database)
        }
      
        buildExtractorsFor(namespace, code, database)
        buildDecomposersFor(namespace, code, database)
      
        buildPackageObjectFor(namespace, code, database, root.properties)
        buildConstantsFor(namespace, code, database)
      }
      
      bundle += dataFile -> code
    }
    
    bundle.create(destPath)
  }
  
  private def buildDataFor(definition: XDefinition, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    def coproductPrefix(x: XCoproduct): String = if (database.productChildrenOf(x).map(_.namespace).removeDuplicates.length <= 1) "sealed " else ""
    def buildProductFields(x: XProduct): CodeBuilder = {
      code.add(x.realFields.map(typeSignatureOf(_)).mkString(", "))
    }
    def buildCoproductFields(x: XCoproduct): CodeBuilder = {
      val commonFields = database.commonFieldsOf(x)
      
      code.join(commonFields, code.newline) { field =>
        code += ("def " + field._1 + ": " + field._2.typename)
      }
      
      code
    }    
    def buildOrderedDefinition(x: XProduct): CodeBuilder = {
      def buildComparisonFor(field: XRealField, schema: XSchema): CodeBuilder = {
        def comparisonSign = field.order match {
          case Ascending  => 1
          case Descending => -1
          case Ignore     => 0
        }
        
        def buildStandardComparison(): CodeBuilder = {
          code.addln("c = this.${field}.compare(that.${field})", "field" -> field.name)
          code.addln("if (c != 0) return c * " + comparisonSign.toString)
        }
                                 
        schema match {
          case x: XOptional   => buildComparisonFor(field, x.optionalType)
          case x: XCollection => code
          case x: XMap        => code
          case x: XTuple      => buildStandardComparison()
          
          case x: XPrimitive  => buildStandardComparison()
          case x: XReference  => buildStandardComparison()
          
          case x: XProduct    => error("Found definition in field")
          case x: XCoproduct  => error("Found definition in field")
          case x: XConstant   => error("Found definition in field")
          case x: XRoot       => error("Found root in field")
          case x: XField      => error("Found field in field")
        }
      }
      
      code.add("def compare(that: " + x.name + "): Int = ").block {    
        code.addln("if (this == that) return 0").newline.addln("var c: Int = 0").newline
      
        code.join(x.realFields, code.newline) { field =>
          buildComparisonFor(field, field.fieldType)
        }
      
        code.newline.add("return this.hashCode - that.hashCode") // TODO: Not good practice, but do we have a choice???
      }
    }
    def buildViewFields(x: XProduct): CodeBuilder = {
      // code.add(x.realFields.map(typeSignatureOf(_)).mkString(", "))
      code.join(x.viewFields, code.newline) { viewField =>
        database.resolve(viewField.fieldType) match {
          case product: XProduct => 
            code.add("def ${field}: ${type} = ${type}(",
              "field" -> viewField.name,
              "type"  -> product.qualifiedName
            )

            code.add(product.realFields.map(_.name).mkString(", "))

            code.add(")")
            
          case _ => error("View type cannot be anything but product: " + viewField.fieldType)
        }
      }
    }
    def formMixinsClauseFromProperty(prop: String): String = definition.properties.get(prop) match {
      case None => ""
      case Some(traits) => " with " + traits.split(",(?=[ \\w+]\\.)").map(_.trim).mkString(" with ")
    }
    
    buildDocumentationFor(definition.properties, code)
    
    val classMixins = formMixinsClauseFromProperty("scala.class.traits")
    
    code.using("type" -> definition.name) {
      definition match {
        case x: XProduct =>
          code.add("case class ${type}(")
          buildProductFields(x)
          code.add(")")
      
          val withClauses = ("Ordered[${type}]" :: database.coproductContainersOf(x).map(_.qualifiedName)).mkString(" with ") + classMixins
      
          code.add(" extends " + withClauses + " ").block {        
            buildOrderedDefinition(x)
            
            code.newline
            
            buildViewFields(x)
          }
      
        case x: XCoproduct => 
          val withClauses = "Product" + classMixins
          
          code.add(coproductPrefix(x) + "trait ${type} extends " + withClauses + " ").block {
            buildCoproductFields(x)
          }
          
        case x: XConstant => code
      }
      
      val objectMixins: String = formMixinsClauseFromProperty("scala.object.traits")
      
      val withClauses = "XSchemaAST.XSchemaDerived" + objectMixins
      
      code.newline.add("object ${type} extends " + withClauses + " ").block {
        code.addln("import XSchemaAST.{XDefinition, XSchema}").newline
        
        code.add("lazy val xschema: XDefinition = " + compact(renderScala(definition.asInstanceOf[XSchema].serialize)) + ".deserialize[XSchema].asInstanceOf[XDefinition]")
      }
    }
  }
  
  private def buildDocumentationFor(properties: Map[String, String], code: CodeBuilder): CodeBuilder = properties.get(PredefinedProperties.XSchemaDoc) match {
    case None => code
    case Some(doc) => code.add("/** ").wrap(doc.replaceAll("\\s+", " "), " * ", 80).newline.add(" */").newline
  }
  
  private def buildExtractorsFor(namespace: Namespace, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    code.newline(2).add("trait Extractors extends DefaultExtractors with ExtractionHelpers ").block {    
      code.join(database.coproductsIn(namespace) ++ database.productsIn(namespace), code.newline.newline) { definition =>
        definition match {
          case x: XProduct => 
            code.using("type" -> x.name) {
              code.add("implicit val ${type}Extractor: Extractor[${type}] = new Extractor[${type}] ").block {
                code.add("def extract(jvalue: JValue): ${type} = ").block {
                  code.add("${type}").paren {          
                    var isFirst = true
          
                    code.join(x.realFields, code.add(",").newline) { field =>
                      code.add("extractField[${fieldType}](jvalue, \"${fieldName}\", " + compact(renderScala(field.defValue)) + ")", 
                        "fieldType" -> field.fieldType.typename,
                        "fieldName" -> field.name
                      )
                    }
                  }
                }
              }
            }
          
          case x: XCoproduct =>
            code.using("type" -> x.name) {
              code.add("private lazy val ${type}ExtractorFunction: PartialFunction[JField, ${type}] = List[PartialFunction[JField, ${type}]]").paren {            
                code.join(x.types, code.add(",").newline) { typ =>
                  code.add("""{ case JField("${productName}", value) => ${productNamespace}.Serialization.${productName}Extractor.extract(value) }""",
                    "productName"      -> typ.name,
                    "productNamespace" -> typ.namespace.value
                  )
                }
              }
            
              code.add(".reduceLeft { (a, b) => a.orElse(b) }").newline
            
              code.add("""
                implicit val ${type}Extractor: Extractor[${type}] = new Extractor[${type}] {
                  def extract(jvalue: JValue): ${type} = {
                    (jvalue --> classOf[JObject]).obj.filter(${type}ExtractorFunction.isDefinedAt _) match {
                      case field :: fields => ${type}ExtractorFunction(field)
                      case Nil => error("Expected to find ${type} but found " + jvalue)
                    }
                  }
                }""")
            }
          
          case x: XConstant =>
          
        }
      }
    }
  }
  
  private def buildDecomposersFor(namespace: Namespace, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    code.newline(2).add("trait Decomposers extends DefaultDecomposers with DecomposerHelpers ").block {    
      code.join(database.coproductsIn(namespace) ++ database.productsIn(namespace), code.newline.newline) { definition =>
        definition match {
          case x: XProduct => 
            code.using("type" -> x.name) {
              code.add("implicit val ${type}Decomposer: Decomposer[${type}] = new Decomposer[${type}] ").block {
                code.add("def decompose(tvalue: ${type}): JValue = ").block {
                  code.add("JObject").paren {      
                    var isFirst = true
      
                    code.join(x.realFields, code.newline) { field =>
                      code.add("JField(\"${fieldType}\", tvalue.${fieldType}.serialize) ::", "fieldType" -> field.name)
                    }
      
                    code.add(" Nil")
                  }
                }
              }
            }
        
          case x: XCoproduct =>
            code.using("type" -> x.name) {
              code.add("implicit val ${type}Decomposer: Decomposer[${type}] = new Decomposer[${type}] ").block {
                code.add("def decompose(tvalue: ${type}): JValue = ").block {
                  code.add("tvalue match ").block {
                    code.join(x.types, code.newline) { typ =>
                      code.add("case x: ${productType} => JObject(JField(\"${productName}\", ${productNamespace}.Serialization.${productName}Decomposer.decompose(x)) :: Nil)",
                        "productName"      -> typ.name,
                        "productType"      -> typ.typename,
                        "productNamespace" -> typ.namespace.value
                      )
                    }
                  }
                }
              }
            }
          
          case x: XConstant =>
        }
      }
    }
  }
  
  private def buildPackageObjectFor(namespace: Namespace, code: CodeBuilder, database: XSchemaDatabase, properties: Map[String, String]): CodeBuilder = {
    val subroot = XRoot(database.definitionsIn(namespace), properties)
    
    code.newline(2).add("object Serialization extends SerializationImplicits with Decomposers with Extractors with XSchemaAST.XSchemaDerived ").block {
      code.addln("import XSchemaAST.{XRoot, XSchema}").newline
      
      // Storing the root as text is not efficient but ensures we do not run 
      // into method size limitations of the JVM (root can be quite large):
      code.add("lazy val xschema: XRoot = parse(\"\"\"" + pretty(render(subroot.asInstanceOf[XSchema].serialize)) + "\"\"\").deserialize[XSchema].asInstanceOf[XRoot]")
    }
  }
  
  private def buildConstantsFor(namespace: Namespace, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    code.newline(2).add("object Constants ").block {    
      code.addln("import Serialization._").newline
    
      code.join(database.constantsIn(namespace), code.newline) { constant =>
        buildDocumentationFor(constant.properties, code)
      
        code.add("lazy val " + constant.name + " = ${json}.deserialize[${type}]",
          "json" -> compact(renderScala(constant.defValue)),
          "type" -> constant.constantType.typename
        )
      }
    }
  }
  
  private def typeSignatureOf(x: XSchema): String = walk(x, CodeBuilder.empty, typeSignatureWalker).code
  
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
