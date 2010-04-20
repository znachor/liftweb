package net.liftweb.json.xschema {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

import java.io.{Writer, PrintWriter}

class XSchemaDatabaseExamplesTest extends Runner(XSchemaDatabaseExamples) with JUnit

object XSchemaDatabaseExamples extends Specification {
  import JsonAST._
  import JsonParser._
  import XSchemaAST._
  import XSchemaSerialization._
  import TestSchemas._
  
  
  import codegen.ScalaCodeGenerator
  
  class UnclosablePrintWriter extends java.io.FilterWriter(new PrintWriter(System.out)) {
    override def close() = { }
  }
  
  implicit val writerF: String => Writer  = (s => new UnclosablePrintWriter)
  
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
    
    val coproduct = db.definitionFor(XSchemaReference("ast.numeric.MixedSum")).get.asInstanceOf[XCoproduct]
    
    val commonFields = db.commonFieldsOf(coproduct)
    
    commonFields.length mustEqual 2
    commonFields(0)._1 mustEqual "term1"
    commonFields(0)._2 mustEqual XSchemaReference("ast.numeric.Expr")
    
    commonFields(1)._1 mustEqual "term2"
    commonFields(1)._2 mustEqual XSchemaReference("ast.numeric.Expr")
  }
}

}
