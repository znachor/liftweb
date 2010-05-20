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
  
  "Common primitive fields in products of a coproduct are identified" in {
    val db = XSchemaDatabase(DataSocialGenderSchema)
    
    val coproduct = DataSocialGenderSchema.definitions.filter(_.isInstanceOf[XCoproduct]).map(_.asInstanceOf[XCoproduct]).first
    
    val commonFields = db.commonFieldsOf(coproduct)
    
    commonFields.length mustEqual 1
    commonFields.first._1 mustEqual "text"
    commonFields.first._2 mustEqual XString
  }
}

} 