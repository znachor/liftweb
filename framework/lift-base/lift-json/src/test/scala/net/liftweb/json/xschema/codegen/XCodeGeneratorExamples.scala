package net.liftweb.json.xschema.codegen {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.JsonParser._

class XCodeGeneratorExamplesTest extends Runner(XCodeGeneratorExamples) with JUnit

object XCodeGeneratorExamples extends Specification {
  import _root_.java.io._
  import _root_.net.liftweb.json.xschema.TestSchemas._
  import _root_.net.liftweb.json.xschema.DefaultSerialization._
  
  class UnclosablePrintWriter extends FilterWriter(new PrintWriter(System.out)) {
    override def close() = { }
  }
  
  implicit val writerF: String => Writer = {
    s => {
      println(s + ":")
      
      new UnclosablePrintWriter
    }
  }
  
  ScalaCodeGenerator.generate(XSchemaSchema, "src/main/scala", "src/test/scala")
}

}