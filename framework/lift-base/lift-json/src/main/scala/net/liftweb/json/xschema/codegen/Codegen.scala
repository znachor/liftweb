package net.liftweb.json.xschema.codegen

import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.Validation._
import _root_.net.liftweb.json.xschema.XSchemaAST._
import _root_.net.liftweb.json.xschema.XSchemaDatabase

import java.io.{FileOutputStream, Writer}

import scala.collection.mutable.{Map => MutableMap}

case class State(indentLevel: Int) {
  def indent   = State(indentLevel + 1)
  def unindent = State(indentLevel - 1)
  
  def tab = "  "
  
  def startIndentation = (0 to indentLevel).foldLeft("") { (cur, l) => cur + tab }
  
  def column = tab.length * indentLevel
}

case class CodeGeneration(code: String, state: State) {
  def + (str: String) = CodeGeneration(code + str, state)
  def + (that: CodeGeneration) = CodeGeneration(code + that.code, that.state)
  
  def indent   = CodeGeneration(code + state.tab, state.indent)
  def unindent = CodeGeneration(code, state.unindent)
  def newline  = CodeGeneration(code + "\n" + state.startIndentation, state)
  
  def indented(f: () => String) = (indent + f()).unindent
  
  def blocked(f: () => String) = wrap(" {", "}")(f)
  
  def curlied(f: () => String) = wrap("(", ")")(f)
  
  def bracketed(f: () => String) = wrap("[", "]")(f)
  
  def joined[A](list: Iterable[A], delim: String)(f: A => String) = this + list.map(f).mkString(delim)
  
  private def wrap(start: String, end: String)(f: () => String) = (((this + start).newline.indent + f()).unindent.newline + end).newline
}

object CodeGeneration {
  def empty = new CodeGeneration("", State(0))
}

case class CodeBundle(fileToCG: MutableMap[String, CodeGeneration]) {
  def += (tuple: (String, CodeGeneration)) = {
    val file  = tuple._1
    val oldCG = forFile(file)
    val newCG = tuple._2
    
    fileToCG += file -> (oldCG + newCG)
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
  
  def map(f: (String, String) => (String, String)): Unit = {
    def fp(tuple: (String, CodeGeneration)): (String, CodeGeneration) = {
      val mapped = f(tuple._1, tuple._2.code)
      
      mapped._1 -> CodeGeneration(mapped._2, State(0))
    }
    
    fileToCG.map(fp);
  }
  
  private def forFile(file: String) = if (fileToCG.contains(file)) fileToCG(file) else {
    fileToCG += file -> CodeGeneration.empty
    
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
  def toFile(ns: Namespace, name: String): String = toFile(ns, "Data", "scala")
  
  def generate(root: XRoot, destPath: String)(implicit writerF: String => Writer) = {
    val bundle   = CodeBundle.empty
    val database = XSchemaDatabase(root.definitions)
    
    for (definition <- root.definitions) {
      val file = toFile(definition.namespace, definition.name)
      
      bundle += file -> walk(definition, CodeGeneration.empty, definitionWalker(database)).newline
    }
    
    bundle.create(destPath)
  }
  
  private def typeSignatureOf(x: XSchema): String = walk(x, CodeGeneration.empty, typeSignatureWalker).code
  
  private def definitionWalker(database: XSchemaDatabase) = new XSchemaDefinitionWalker[CodeGeneration] {
    override def begin(data: CodeGeneration, defn: XDefinition) = {
      def coproductPrefix(x: XCoproduct): String = if (database.productChildrenOf(x).map(_.namespace).removeDuplicates.length <= 1) "sealed " else ""
      def productFields(x: XProduct): String = x.fields.map(typeSignatureOf(_)).mkString(", ")
      def coproductFields(cg: CodeGeneration, x: XCoproduct): CodeGeneration = {
        val commonFields = database.commonFieldsOf(x)
        
        commonFields.foldLeft(cg) { (cg, field) => (cg + "def " + field._1 + ": " + field._2.typename).newline }
      }
      
      val packagePrefix = (data.newline + "package " + defn.namespace.value + " {").indent.newline
      
      (defn match {
        case x: XProduct   => packagePrefix + "case class " + defn.name + "(" + productFields(x) + ")"
        case x: XCoproduct => coproductFields((packagePrefix + coproductPrefix(x) + "trait " + x.name + " {").indent.newline, x).unindent.newline + "}"
      })
    }
    
    override def end(data: CodeGeneration, defn: XDefinition) = {
      (data + (defn match {
        case x: XProduct => {
          val withClauses = database.coproductContainersOf(x).map(_.qualifiedName).mkString(" with ")
          
          val extensions = if (withClauses.length > 0) " extends " + withClauses else ""
          
          extensions
        }
        case x: XCoproduct => ""
      })).unindent.newline + "}"
    }
  }
  
  private def decomposingWalker(database: XSchemaDatabase) = new XSchemaDefinitionWalker[CodeGeneration] {
    override def begin(data: CodeGeneration, defn: XDefinition) = {
      def coproductPrefix(x: XCoproduct): String = if (database.productChildrenOf(x).map(_.namespace).removeDuplicates.length <= 1) "sealed " else ""
      def productFields(x: XProduct): String = x.fields.map(typeSignatureOf(_)).mkString(", ")
      def coproductFields(cg: CodeGeneration, x: XCoproduct): CodeGeneration = {
        val commonFields = database.commonFieldsOf(x)
        
        commonFields.foldLeft(cg) { (cg, field) => (cg + "def " + field._1 + ": " + field._2.typename).newline }
      }
      
      val packagePrefix = (data.newline + "package " + defn.namespace.value + " {").indent.newline
      
      (defn match {
        case x: XProduct   => packagePrefix + "case class " + defn.name + "(" + productFields(x) + ")"
        case x: XCoproduct => coproductFields((packagePrefix + coproductPrefix(x) + "trait " + x.name + " {").indent.newline, x).unindent.newline + "}"
      })
    }
    
    override def end(data: CodeGeneration, defn: XDefinition) = {
      (data + (defn match {
        case x: XProduct => {
          val withClauses = database.coproductContainersOf(x).map(_.qualifiedName).mkString(" with ")
          
          val extensions = if (withClauses.length > 0) " extends " + withClauses else ""
          
          extensions
        }
        case x: XCoproduct => ""
      })).unindent.newline + "}"
    }
  }
  
  private lazy val typeSignatureWalker = new XSchemaDefinitionWalker[CodeGeneration] {
    override def begin(data: CodeGeneration, field: XFieldDefinition) = {
      data + field.name + ": "
    }
    
    override def begin(data: CodeGeneration, opt: XOptional) = {
      data + "Option["
    }
    
    override def begin(data: CodeGeneration, col: XCollection) = {
      data + (col.collection match {
        case XSet   => "Set"
        case XArray => "Array"
        case XList  => "List"
      }) + "["
    }
    
    override def begin(data: CodeGeneration, map: XMap) = {
      data + "Map[String, "
    }
    
    override def begin(data: CodeGeneration, tuple: XTuple) = {
      data + "(" + tuple.types.map(typeSignatureOf(_)).mkString(", ") + ")"
    }
    
    override def walk(data: CodeGeneration, prim: XPrimitive) = {
      data + (prim match {
        case XString  => "String"
        case XInt     => "Int"
        case XLong    => "Long"
        case XFloat   => "Float"
        case XDouble  => "Double"
        case XBoolean => "Boolean"
      })
    }
    
    override def walk(data: CodeGeneration, ref: XReference) = {
      data + ref.typename
    }
    
    override def end(data: CodeGeneration, opt: XOptional) = {
      data + "]"
    }
    
    override def end(data: CodeGeneration, col: XCollection) = {
      data + "]"
    }
    
    override def end(data: CodeGeneration, map: XMap) = {
      data + "]"
    }
  }
}
