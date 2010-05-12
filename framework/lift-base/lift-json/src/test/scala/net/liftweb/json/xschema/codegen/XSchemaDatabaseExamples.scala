package net.liftweb.json.xschema.codegen {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

import java.io.{Writer, PrintWriter}

class XSchemaDatabaseExamplesTest extends Runner(XSchemaDatabaseExamples) with JUnit

object XSchemaDatabaseExamples extends Specification {
  import _root_.net.liftweb.json.JsonAST._
  import _root_.net.liftweb.json.JsonParser._
  import _root_.net.liftweb.json.xschema._
  import _root_.net.liftweb.json.xschema.DefaultSerialization._
  import _root_.net.liftweb.json.xschema.TestSchemas._
    
  class UnclosablePrintWriter extends java.io.FilterWriter(new PrintWriter(System.out)) {
    override def close() = { }
  }
  
  implicit val writerF: String => Writer = {
    s => {
      println(s + ":")
      
      new UnclosablePrintWriter
    }
  }
  
  ScalaCodeGenerator.generate(DataSocialGenderSchema, ".")
  
  "Common primitive fields in products of a coproduct are identified" in {
    val db = XSchemaDatabase(DataSocialGenderSchema)
    
    val coproduct = DataSocialGenderSchema.definitions.filter(_.isInstanceOf[XCoproduct]).map(_.asInstanceOf[XCoproduct]).first
    
    val commonFields = db.commonFieldsOf(coproduct)
    
    commonFields.length mustEqual 1
    commonFields.first._1 mustEqual "text"
    commonFields.first._2 mustEqual XString
  }
  
  "Common fields in products of a coproduct with compatible types are unified" in {
    val db = XSchemaDatabase(AstNumericExprSchema)
    
    val coproduct = db.definitionFor(XDefinitionRef("MixedSum", "ast.numeric")).get.asInstanceOf[XCoproduct]
    
    val commonFields = db.commonFieldsOf(coproduct)
    
    commonFields.length mustEqual 2
    commonFields(0)._1 mustEqual "term1"
    commonFields(0)._2 mustEqual XDefinitionRef("Expr", "ast.numeric")
    
    commonFields(1)._1 mustEqual "term2"
    commonFields(1)._2 mustEqual XDefinitionRef("Expr", "ast.numeric")
  }
}

}
