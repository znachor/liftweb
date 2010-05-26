package net.liftweb.json.xschema.codegen

import _root_.net.liftweb.json.JsonParser
import _root_.net.liftweb.json.xschema.{XRoot}
import _root_.net.liftweb.json.xschema.Serialization._

import java.lang.StringBuilder
import java.io.{Writer}

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
  
  def main(args: Array[String]) {
    import java.io._
    
    try {
      if (args.length != 3) {
        println("Usage: [xschema file] [dest code path] [dest tests path]")
      }
      else {
        def using[T <: Closeable, S](c: => T)(f: T => S): S = { val resource = c; try { f(resource) } finally { resource.close } }
      
        using(new DataInputStream(new FileInputStream(args(0)))) { stream =>
          val json = stream.readUTF
          
          implicit def writerF(file: String): Writer = new FileWriter(file)
        
          generate(JsonParser.parse(json).deserialize[XRoot], args(1), args(2))
        
          println("Successfully generated code at " + args(1) + " and tests at " + args(2))
        
          System.exit(0)
        }
      }
    }
    catch {
      case t: Throwable => 
        println("Encountered error during code generation: " + t.getMessage)
        
        System.exit(1)
    }
  }
}