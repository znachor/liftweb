package net.liftweb.json.xschema.codegen

import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.Validation._
import _root_.net.liftweb.json.Printer._
import _root_.net.liftweb.json.xschema._
import _root_.net.liftweb.json.xschema.XSchemaTree._
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
    def replace(template: String, replacements: Iterable[(String, String)]) = replacements.foldLeft(template) { (t, r) => t.replace("${" + r._1 + "}", r._2) }
    
    var newStr = replacements.toList.foldLeft(str) { (str, replacement) => replace(str, replacement) }
    
    var isFirst = true
    
    var indents = new ArrayStack[Int]
    
    var startIndentLevel = state.indentLevel
    
    var lines = newStr.split("\n").toList match {
      case x :: xs if (x.trim.length == 0) => xs
      case xs => xs
    }
    
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
  
  def add(template: String, replacements: (String, String)*): CodeBuilder = using[CodeBuilder](replacements: _*) { this += template }
  
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
  def toDirectory(ns: String) = ns.replace(".", "/") + "/"

  def toFile(ns: String, name: String, extension: String): String = toDirectory(ns) + name + "." + extension
}

trait CodeGenerator {
  def generate(root: XRoot, destPathCode: String, destPathTests: String)(implicit writerF: String => Writer)
}


object ScalaCodeGenerator extends CodeGenerator with CodeGeneratorHelpers {
  def generate(root: XRoot, destPathCode: String, destPathTests: String)(implicit writerF: String => Writer) = {
    val includeSchemas = root.properties.get(PredefinedProperties.XSchemaIncludeSchemas).map(_.toLowerCase).map { 
      case "true" => true
      case _ => false
    }.getOrElse(true)
    
    val bundle   = CodeBundle.empty
    val database = XSchemaDatabase(root.definitions)
    
    for (namespace <- database.namespaces) {
      val code = CodeBuilder.empty
      val test = CodeBuilder.empty
      
      buildCodeFor(namespace, code, root, database, includeSchemas)
      buildTestFor(namespace, code, root, database, includeSchemas)
      
      bundle += toFile(namespace, "Data", "scala")     -> code
      bundle += toFile(namespace, "DataTest", "scala") -> test
    }
    
    bundle.create(destPathCode)
  }
  
  private def buildCodeFor(namespace: String, code: CodeBuilder, root: XRoot, database: XSchemaDatabase, includeSchemas: Boolean): Unit = {
    code.addln("// This code was auto-generated by Lift Json XSchema - do not edit")
    
    code.add("package " + namespace + " ").block {
      code.addln("import net.liftweb.json.JsonParser._").
           addln("import net.liftweb.json.JsonAST._")
           
      if (namespace != "net.liftweb.json.xschema") {
        code.addln("import net.liftweb.json.xschema.{SerializationImplicits, Extractor, ExtractionHelpers, Decomposer, DecomposerHelpers, DefaultExtractors, DefaultDecomposers, DefaultOrderings}")
        
        if (includeSchemas) {
          code.addln("import net.liftweb.json.xschema.{XRoot, XProduct, XCoproduct, XSchemaDerived}")
        }
      }
           
      code.addln("import net.liftweb.json.xschema.DefaultSerialization._")
           
      root.properties.get("scala.imports") match {
        case None => 
        case Some(imports) => imports.split(",(?![^{]+[}])").map("import " + _.trim) foreach { x => code.addln(x) }
      }
      
      val otherReferencedNamespaces = database.definitionsReferencedIn(namespace).map(_.namespace).filter(_ != namespace)
      
      if (otherReferencedNamespaces.length > 0) {      
        code.newline
        
        code.join(otherReferencedNamespaces, code.newline) { externalNamespace =>
          code.add("import " + externalNamespace + ".Serialization._")
        }
      }
      
      buildOrderingsFor(namespace, code, database)
      
      code.newline(2)
    
      code.join(database.coproductsIn(namespace) ++ database.productsIn(namespace), code.newline.newline) { definition =>
        buildDataFor(definition, code, database, includeSchemas)
      }
    
      buildExtractorsFor(namespace, code, database)
      buildDecomposersFor(namespace, code, database)
      buildPackageObjectFor(namespace, code, database, root.properties, includeSchemas)
      
      buildConstantsFor(namespace, code, database)
    }
  }
  
  private def buildTestFor(namespace: String, code: CodeBuilder, root: XRoot, database: XSchemaDatabase, includeSchemas: Boolean): Unit = {
    code.addln("// These tests were auto-generated by Lift Json XSchema - do not edit")
    
    code.using("namespace" -> namespace) {
      code.newline.add("package " + namespace + " ").block {
        code.add("""
          import _root_.org.specs.Specification
          import _root_.org.specs.runner.{Runner, JUnit}

          import net.liftweb.json.JsonParser._
          import net.liftweb.json.JsonAST._

          import ${namespace}.Serialization._
          import ${namespace}.Constants._
        """).newline
        
        code.add("import ${namespace}.{")
        
        code.join((database.coproductsIn(namespace) ++ database.productsIn(namespace)).map(_.name), code.add(", ")) { name =>
          code.add(name)
        }
        
        code.addln("}").newline
        
        buildSerializationTestFor(namespace, code, root, database, includeSchemas)
      }
    }
  }
  
  private def buildSerializationTestFor(namespace: String, code: CodeBuilder, root: XRoot, database: XSchemaDatabase, includeSchemas: Boolean): Unit = {
    code.addln("class DataSerializationTest extends Runner(DataSerializationExamples) with JUnit")
    
    code.add("object DataSerializationExamples extends Specification ").block {
      code.join(database.coproductsIn(namespace) ++ database.productsIn(namespace), code.newline.newline) { defn =>
        defn match { 
          case x: XProduct if (x.isSingleton) => 
            // Due to apparent bug in Scala compiler (implicits for singleton types), we must treat this case specially:
            code.add("""
              "Deserialization of ${name} will not fail even when information is missing" in {
                Extractors.${name}Extractor.extract(JObject(Nil)).isInstanceOf[${type}] must be (true)
              }

              "Serialization of ${name} has non-zero information content" in {
                Decomposers.${name}Decomposer.decompose(Extractors.${name}Extractor.extract(JObject(Nil))).isInstanceOf[${type}] mustNot be (JObject(Nil))
              }
            """, "name" -> defn.name, "type" -> typeSignatureOf(defn.referenceTo, database))
          
          case _ => 
            code.add("""
              "Deserialization of ${name} will not fail even when information is missing" in {
                JObject(Nil).deserialize[${type}].isInstanceOf[${type}] must be (true)
              }

              "Serialization of ${name} has non-zero information content" in {
                JObject(Nil).deserialize[${type}].serialize mustNot be (JObject(Nil))
              }
            """, "name" -> defn.name, "type" -> typeSignatureOf(defn.referenceTo, database))
        }
      }
      /*
      
      */
      
    }
  }
  
  private def buildOrderingTestFor(namespace: String, code: CodeBuilder, root: XRoot, database: XSchemaDatabase, includeSchemas: Boolean): Unit = {
    code.addln("class DataOrderingTest extends Runner(DataOrderingExamples) with JUnit")
    
    code.add("object DataOrderingExamples extends Specification ").block {
      
    }
  }
  
  private def buildDataFor(definition: XDefinition, code: CodeBuilder, database: XSchemaDatabase, includeSchemas: Boolean): Unit = {
    def coproductPrefix(x: XCoproduct): String = if (database.namespacesOf(x).removeDuplicates.length <= 1) "sealed " else ""
    def buildProductFields(x: XProduct): Unit = {
      code.add(x.realFields.map(x => x.name + ": " + typeSignatureOf(x.fieldType, database)).mkString(", "))
    }
    def buildCoproductFields(x: XCoproduct): Unit = {
      val commonFields = database.commonFieldsOf(x)
      
      code.join(commonFields, code.newline) { field =>
        code += ("def " + field._1 + ": " + typeSignatureOf(field._2, database))
      }
    }    
    def buildOrderedDefinition(x: XProduct): Unit = {
      def buildComparisonFor(field: XRealField, reference: XReference): Unit = {
        def comparisonSign = field.order match {
          case XOrderAscending  => 1
          case XOrderDescending => -1
          case XOrderIgnore     => 0
        }
        
        def buildStandardComparison(): Unit = {
          code.addln("c = this.${field}.compare(that.${field})", "field" -> field.name)
          code.addln("if (c != 0) return c * " + comparisonSign.toString)
        }
                                 
        database.resolve(reference) match {
          case x: XProduct =>
            if (x.isSingleton) { // TODO: Should not solve this here, should solve it in "Orderings"
              code.addln("if (this.${field} == that.${field}) return 0")
            }
            else buildStandardComparison()
            
          case _ => buildStandardComparison()
        }
      }
      
      code.add("def compare(that: " + typeSignatureOf(x.referenceTo, database) + "): Int = ").block {    
        code.add("import Orderings._").newline(2)
        
        code.addln("if (this == that) return 0").newline.addln("var c: Int = 0").newline
      
        code.join(x.realFields, code.newline) { field =>
          buildComparisonFor(field, field.fieldType)
        }
      
        code.newline.add("return this.hashCode - that.hashCode") // TODO: Not good practice, but do we have a choice???
      }
    }
    def buildViewFields(x: XProduct): Unit = {
      code.join(x.viewFields, code.newline) { viewField =>
        database.resolve(viewField.fieldType) match {
          case product: XProduct => 
            code.using("field" -> viewField.name, "type" -> typeSignatureOf(viewField.fieldType, database)) {
              if (product.isSingleton) {
                error("View fields cannot be used for objects, constant fields should be used instead")
              }
              else {
                code.add("def ${field}: ${type} = ${type}(")
                code.add(product.realFields.map(_.name).mkString(", "))
                code.add(")")
              }
            }
            
          case x => error("View type cannot be anything but product: " + x)
        }
      }
    }
    def buildConstantFields(x: XProduct): Unit = {
      code.join(x.constantFields, code.newline) { constantField =>
        database.resolve(constantField.fieldType) match {
          case product: XProduct => 
            code.using("field" -> constantField.name, "type"  -> typeSignatureOf(product.referenceTo, database)) {
              code.add("lazy val ${field}: ${type} = " + compact(renderScala(constantField.default)) + ".deserialize[${type}]")
            }
            
          case x => error("Constant type cannot be anything but product: " + x)
        }
      }
    }
    def formMixinsClauseFromProperty(prop: String): List[String] = definition.properties.get(prop) match {
      case None => Nil
      case Some(traits) => 
        traits.split(",(?=[ \\w]+\\.)").toList.map(_.trim).flatMap { x =>
          x match {
            case ""  => Nil
            case str => str :: Nil
          }
        }
    }
    def buildXSchema() = {
      code.add("lazy val xschema: ${definitionType} = net.liftweb.json.xschema.Extractors.${definitionType}Extractor.extract(" + compact(renderScala(definition.serialize)) + ")",
        "definitionType" -> (definition match { case _ : XProduct => "XProduct"; case _ => "XCoproduct" })
      )
    }
    
    buildDocumentationFor(definition.properties, code)
    
    val classMixins = formMixinsClauseFromProperty("scala.class.traits")
    
    var isSingleton = definition match {
      case x: XProduct => x.isSingleton
      case _           => false
    }
    
    code.using("type" -> typeSignatureOf(definition.referenceTo, database), "name" -> definition.name) {
      definition match {
        case x: XProduct =>
          var initialExtends: List[String] = Nil
          
          if (isSingleton) {
            code.add("case object ${name}")
            
            if (includeSchemas) {
              initialExtends = List("XSchemaDerived")
            }
          }
          else {
            code.add("case class ${name}(")
            buildProductFields(x)
            code.add(")")
            
            initialExtends = List("Ordered[${type}]")
          }
      
          val withClauses = initialExtends ::: database.coproductContainersOf(x).map { x => typeSignatureOf(x.referenceTo, database) } ::: classMixins

          if (withClauses.length > 0) {
            code.add(" extends " + withClauses.mkString(" with ") + " ")
          }
          
          if (!isSingleton) {
            code.block {
              buildOrderedDefinition(x)
            
              code.newline
            
              buildViewFields(x)
            }
          }
          else if (includeSchemas) {
            code.block {
              buildXSchema()
            }
          }
      
        case x: XCoproduct => 
          val withClauses = "Product" :: database.coproductContainersOf(x).map { x => typeSignatureOf(x.referenceTo, database) } ::: classMixins
          
          code.add(coproductPrefix(x) + "trait ${name} extends " + withClauses.mkString(" with ") + " ").block {
            buildCoproductFields(x)
          }
          
        case x: XConstant => 
      }
      
      // **********
      val objectMixins = formMixinsClauseFromProperty("scala.object.traits")
      
      if (!isSingleton && (includeSchemas || objectMixins.length > 0)) {
        val withClauses: List[String] = if (includeSchemas) "XSchemaDerived" :: objectMixins else objectMixins
      
        code.newline.add("object ${name} extends " + withClauses.mkString(" with ") + " ").block {
          buildXSchema()
        }
      }
    }
  }
  
  private def buildDocumentationFor(properties: Map[String, String], code: CodeBuilder): Unit = properties.get(PredefinedProperties.XSchemaDoc) match {
    case None => 
    case Some(doc) => code.add("/** ").wrap(doc.replaceAll("\\s+", " "), " * ", 80).newline.add(" */").newline
  }
  
  private def buildExtractorsFor(namespace: String, code: CodeBuilder, database: XSchemaDatabase): CodeBuilder = {
    code.newline(2).add("trait Extractors extends DefaultExtractors with ExtractionHelpers ").block {    
      code.join(database.coproductsIn(namespace) ++ database.productsIn(namespace), code.newline.newline) { definition =>
        definition match {
          case x: XProduct => 
            code.using("name" -> x.name, "type" -> typeSignatureOf(x.referenceTo, database)) {
              code.add("implicit val ${name}Extractor: Extractor[${type}] = new Extractor[${type}] ").block {
                code.add("def extract(jvalue: JValue): ${type} = ").block {
                  if (x.realFields.length == 0) {
                    code.add("${name}")
                  }
                  else {
                    code.add("${name}").paren {          
                      var isFirst = true
          
                      code.join(x.realFields, code.add(",").newline) { field =>
                        code.add("extractField[${fieldType}](jvalue, \"${fieldName}\", " + compact(renderScala(field.default)) + ")", 
                          "fieldType" -> typeSignatureOf(field.fieldType, database),
                          "fieldName" -> field.name
                        )
                      }
                    }
                  }
                }
              }
            }
          
          case x: XCoproduct =>
            code.using("name" -> x.name, "type" -> typeSignatureOf(x.referenceTo, database)) {
              code.add("private lazy val ${name}ExtractorFunction: PartialFunction[JField, ${type}] = ").block {
                code.join(x.terms, code.newline) { typ =>
                  code.add("""case JField("${subtypeName}", value) => ${subtypeString}.Serialization.${subtypeName}Extractor.extract(value)""",
                    "subtypeName"   -> typ.name,
                    "subtypeString" -> typ.namespace
                  )
                }
              }
            
              code.newline.add("""
                implicit val ${name}Extractor: Extractor[${type}] = new Extractor[${type}] {
                  def extract(jvalue: JValue): ${type} = {
                    (jvalue --> classOf[JObject]).obj.filter(${name}ExtractorFunction.isDefinedAt _) match {
                      case field :: fields => ${name}ExtractorFunction(field)
                      case Nil => error("Expected to find ${type} but found " + jvalue)
                    }
                  }
                }""")
            }
        }
      }
    }
    code.newline.add("object Extractors extends Extractors")
  }
  
  private def buildDecomposersFor(namespace: String, code: CodeBuilder, database: XSchemaDatabase): Unit = {
    code.newline(2).add("trait Decomposers extends DefaultDecomposers with DecomposerHelpers ").block {    
      code.join(database.coproductsIn(namespace) ++ database.productsIn(namespace), code.newline.newline) { definition =>
        definition match {
          case x: XProduct => 
            code.using("name" -> x.name, "type" -> typeSignatureOf(x.referenceTo, database)) {
              code.add("implicit val ${name}Decomposer: Decomposer[${type}] = new Decomposer[${type}] ").block {
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
            code.using("name" -> x.name, "type" -> typeSignatureOf(x.referenceTo, database)) {
              code.add("implicit val ${name}Decomposer: Decomposer[${type}] = new Decomposer[${type}] ").block {
                code.add("def decompose(tvalue: ${type}): JValue = ").block {
                  code.add("tvalue match ").block {
                    code.join(x.terms, code.newline) { typ =>
                      code.add("case x: ${productType} => JObject(JField(\"${productName}\", ${productString}.Serialization.${productName}Decomposer.decompose(x)) :: Nil)",
                        "productName"      -> typ.name,
                        "productType"      -> typeSignatureOf(typ, database),
                        "productString" -> typ.namespace
                      )
                    }
                  }
                }
              }
            }
        }
      }
    }
    code.newline.add("object Decomposers extends Decomposers")
  }
  
  private def buildConstantsFor(namespace: String, code: CodeBuilder, database: XSchemaDatabase): Unit = {
    code.newline(2).add("object Constants ").block {    
      code.addln("import Serialization._").newline
    
      code.join(database.constantsIn(namespace), code.newline) { constant =>
        buildDocumentationFor(constant.properties, code)
      
        code.add("lazy val " + constant.name + " = ${json}.deserialize[${type}]",
          "json" -> compact(renderScala(constant.default)),
          "type" -> typeSignatureOf(constant.constantType, database)
        )
      }
    }
  }
  
  private def buildOrderingsFor(namespace: String, code: CodeBuilder, database: XSchemaDatabase): Unit = {
    code.newline(2).add("trait Orderings ").block {
      code.join(database.coproductsIn(namespace), code.newline) { coproduct => 
        code.using("type" -> coproduct.name) {
          code.add("implicit def ${type}ToOrdered${type}(inner: ${type}) = Ordered${type}(inner)")
        }
      }
      
      code.newline(2)
      
      code.join(database.coproductsIn(namespace), code.newline(2)) { coproduct => 
        code.using("type" -> typeSignatureOf(coproduct.referenceTo, database), "name" -> coproduct.name) {
          code.add("case class Ordered${name}(inner: ${type}) extends Ordered[${type}] ").block {
            code.add("def compare(that: ${type}): Int = ").block {
              code.addln("if (inner == that) 0")
              code.add("else inner match ").block {
                code.join(coproduct.terms, code.newline) { subtype1 => 
                  code.add("case x: ${subtype1} => that match ", "subtype1" -> typeSignatureOf(subtype1, database)).block {
                    code.join(coproduct.terms, code.newline) { subtype2 => 
                      val index1 = coproduct.terms.indexOf(subtype1)
                      val index2 = coproduct.terms.indexOf(subtype2)
                      
                      val cmp = if (index1 < index2) "-1"
                                else if (index2 < index1) "1"
                                else "x.compare(y)"
                      
                      code.add("case y: ${subtype2} => " + cmp,
                        "subtype2" -> typeSignatureOf(subtype2, database)
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    code.newline.add("object Orderings extends Orderings")
  }
  
  private def buildPackageObjectFor(namespace: String, code: CodeBuilder, database: XSchemaDatabase, properties: Map[String, String], includeSchemas: Boolean): Unit = {
    val subroot = XRoot(database.definitionsIn(namespace), properties)
    
    code.newline(2).add("object Serialization extends SerializationImplicits with Decomposers with Extractors with Orderings ").block {
      // Storing the root as text is not efficient but ensures we do not run 
      // into method size limitations of the JVM (root can be quite large):
      if (includeSchemas) {
        code.add("lazy val xschema: XRoot = net.liftweb.json.xschema.Extractors.XRootExtractor.extract(parse(\"\"\"" + compact(render(subroot.serialize)) + "\"\"\"))")
      }
    }
  }
  
  private def typeSignatureOf(x: XReference, database: XSchemaDatabase): String = walk(x, CodeBuilder.empty, typeSignatureWalker(database)).code
  
  private def typeSignatureWalker(database: XSchemaDatabase) = new XSchemaWalker[CodeBuilder] {
    override def begin(data: CodeBuilder, opt: XOptional) = {
      data += "Option["
    }
    
    override def begin(data: CodeBuilder, col: XCollection) = {
      data += ((col match {
        case x: XSet   => "Set"
        case x: XArray => "Array"
        case x: XList  => "List"
      }) + "[")
    }
    
    override def begin(data: CodeBuilder, map: XMap) = {
      data += "Map["
    }
    
    override def begin(data: CodeBuilder, tuple: XTuple) = {
      data += "("
    }
    
    override def separator(data: CodeBuilder) = data.add(", ")
    
    override def walk(data: CodeBuilder, prim: XPrimitiveRef) = {
      data += (prim match {
        case XString  => "String"
        case XInt     => "Int"
        case XLong    => "Long"
        case XFloat   => "Float"
        case XDouble  => "Double"
        case XBoolean => "Boolean"
        case XJSON    => "net.liftweb.json.JsonAST.JValue"
      })
    }
    
    override def walk(data: CodeBuilder, defn: XDefinitionRef) = {
      database.resolve(defn) match {
        case x: XProduct =>
          if (x.isSingleton) {
            data += (defn.namespace + "." + defn.name + ".type")
          }
          else {
            data += (defn.namespace + "." + defn.name)
          }

        case _ => data += (defn.namespace + "." + defn.name)
      }
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
    
    override def end(data: CodeBuilder, tuple: XTuple) = {
      data += ")"
    }
  }
}
