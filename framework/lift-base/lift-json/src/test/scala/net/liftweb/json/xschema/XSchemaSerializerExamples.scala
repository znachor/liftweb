package net.liftweb.json.xschema {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.JsonParser._

import XSchemaAST._
import Serialization._

class XSchemaSerializerExamplesTest extends Runner(XSchemaSerializerExamples) with JUnit

object XSchemaSerializerExamples extends Specification {
  import TestSchemas._
  
  "Extraction and decomposition are symmetric" in {
    //println(AstNumericExprSchema.asInstanceOf[XSchema].serialize.deserialize[XSchema])
    //println(AstNumericExprSchema)
    
    AstNumericExprSchema.asInstanceOf[XSchema].serialize.deserialize[XSchema] mustEqual AstNumericExprSchema
  }
  
  "Extraction and decomposition are symmetric 2" in {
    DataSocialGenderSchema.asInstanceOf[XSchema].serialize.deserialize[XSchema] mustEqual DataSocialGenderSchema
  }
}

object TestSchemas {
  val numberExpr = parse("""{ "Number": { "value": 0.0 } }""")
  val number     = parse("""{ "value": 0.0 }""")

  val AstNumericExprSchema = XRoot(
    List(
      XCoproduct(
        Namespace("ast.numeric"),
        "Expr",
        Map(),
        List(
          XReference("ast.numeric.NumericSum"),
          XReference("ast.numeric.Sum"),
          XReference("ast.numeric.Product"),
          XReference("ast.numeric.Number")
        )
      ),
      XCoproduct(
        Namespace("ast.numeric"),
        "MixedSum",
        Map(),
        List(
          XReference("ast.numeric.NumericSum"),
          XReference("ast.numeric.Sum")
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "NumericSum",
        Map(),
        List(
          XRealField(XReference("ast.numeric.Number"), "term1", Map(), numberExpr, Descending),
          XRealField(XReference("ast.numeric.Number"), "term2", Map(), numberExpr, Descending)
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "Sum",
        Map(),
        List(
          XRealField(XReference("ast.numeric.Expr"), "term1", Map(), numberExpr, Descending),
          XRealField(XReference("ast.numeric.Expr"), "term2", Map(), numberExpr, Descending)
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "Product",
        Map(),
        List(
          XRealField(XReference("ast.numeric.Expr"), "term1", Map(), numberExpr, Descending),
          XRealField(XReference("ast.numeric.Expr"), "term2", Map(), numberExpr, Descending)
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "Number",
        Map(),
        List(
          XRealField(XDouble, "value", Map(), number, Descending)
        )
      )
    ),
    Map()
  )

  val DataSocialGenderSchema = XRoot(
    List(
      XCoproduct(
        Namespace("data.social"),
        "Gender",
        Map(
          "xschema.doc" -> "This is the coproduct that includes male and female. The normal way to translate this into OOP is as a superclass/superinterface.",
          "scala.class.traits" -> "java.io.Serializable, java.lang.Cloneable",
          "scala.object.traits" -> "java.io.Serializable, java.lang.Cloneable"
        ),
        List(
          XReference("data.social.Male"),
          XReference("data.social.Female")
        )
      ),
      XProduct(
        Namespace("data.social"),
        "Male",
        Map("scala.class.traits" -> "java.io.Serializable, java.lang.Cloneable"),
        List(
          XRealField(XString, "text", Map(), JString("male"), Descending),
          XViewField(XReference("data.social.Female"), "asFemale", Map())
        )
      ),
      XProduct(
        Namespace("data.social"),
        "Female",
        Map("scala.class.traits" -> "java.io.Serializable, java.lang.Cloneable"),
        List(
          XRealField(XString, "text", Map(), JString("female"), Ascending),
          XViewField(XReference("data.social.Male"), "asMale", Map())
        )
      ),
      XConstant(
        Namespace("data.social"),
        "DefaultFemale",
        Map(),
        XReference("data.social.Gender"),
        JObject(
          JField("Female",
            JObject(
              JField("text", JString("female")) :: Nil
            )
          ) :: Nil
        )
      ),
      XConstant(
        Namespace("data.social"),
        "DefaultMale",
        Map(),
        XReference("data.social.Gender"),
        JObject(
          JField("Male",
            JObject(
              JField("text", JString("male")) :: Nil
            )
          ) :: Nil
        )
      ),
      XProduct(
        Namespace("data.social"),
        "Morning",
        Map(),
        List()
      ),
      XProduct(
        Namespace("data.social"),
        "Noon",
        Map(),
        List()
      ),
      XProduct(
        Namespace("data.social"),
        "Night",
        Map(),
        List()
      ),
      XCoproduct(
        Namespace("data.social"),
        "Time",
        Map(),
        List(
          XReference("data.social.Morning"),
          XReference("data.social.Noon"),
          XReference("data.social.Night")
        )
      )
    ),
    Map(
      "scala.imports" -> "net.liftweb.json.xschema.{SerializationImplicits => XSerializationImplicits, DefaultExtractors => XDefaultExtractors}, java.lang.reflect.*"
    )
  )
  
  val XSchemaSchema = XRoot(
    List(
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "QualifiedName2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XString, "namespace", Map(), JNothing, Ascending)
        )
      ),
      XCoproduct(
        Namespace("net.liftweb.json.xschema2"),
        "XSchema2",
        Map(),
        List(
          XReference("net.liftweb.json.xschema2.XDefinition2"),
          XReference("net.liftweb.json.xschema2.XReference2")
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XReference2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XString, "namespace", Map(), JString(""), Ascending),
          XViewField(XReference("net.liftweb.json.xschema2.QualifiedName2"), "qualifiedName", Map())
        )
      ),
      XCoproduct(
        Namespace("net.liftweb.json.xschema2"),
        "XDefinition2",
        Map(),
        List(
          XReference("net.liftweb.json.xschema2.XField2"),
          XReference("net.liftweb.json.xschema2.XProduct2"),
          XReference("net.liftweb.json.xschema2.XCoproduct2"),
          XReference("net.liftweb.json.xschema2.XConstant2")
        )
      ),
      XCoproduct(
        Namespace("net.liftweb.json.xschema2"),
        "XField2",
        Map(),
        List(
          XReference("net.liftweb.json.xschema2.XRealField2"),
          XReference("net.liftweb.json.xschema2.XViewField2"),
          XReference("net.liftweb.json.xschema2.XConstantField2")
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XProduct2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XString, "namespace", Map(), JString(""), Ascending),
          XViewField(XReference("net.liftweb.json.xschema2.QualifiedName2"), "qualifiedName", Map()),
          XRealField(XMap(XString, XString), "properties", Map(), JArray(Nil), Ascending),
          XRealField(XCollection(XReference("net.liftweb.json.xschema2.XReference2"), XList), "terms", Map(), JObject(Nil), Ascending)
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XCoproduct2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XString, "namespace", Map(), JString(""), Ascending),
          XViewField(XReference("net.liftweb.json.xschema2.QualifiedName2"), "qualifiedName", Map()),
          XRealField(XMap(XString, XString), "properties", Map(), JArray(Nil), Ascending),
          XRealField(XCollection(XReference("net.liftweb.json.xschema2.XReference2"), XList), "terms", Map(), JObject(Nil), Ascending)
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XConstant2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XString, "namespace", Map(), JString(""), Ascending),
          XViewField(XReference("net.liftweb.json.xschema2.QualifiedName2"), "qualifiedName", Map()),
          XRealField(XMap(XString, XString), "properties", Map(), JArray(Nil), Ascending),
          XRealField(XJValue, "default", Map(), JNothing, Ascending)
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XRealField2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XMap(XString, XString), "properties", Map(), JArray(Nil), Ascending),
          XRealField(XJValue, "default", Map(), JNothing, Ascending),
          XRealField(XReference("net.liftweb.json.xschema2.XOrder2"), "order", Map(), JObject(JField("XOrderAscending2", JObject(Nil)) :: Nil), Ascending)
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XViewField2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XMap(XString, XString), "properties", Map(), JArray(Nil), Ascending)
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XConstantField2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XMap(XString, XString), "properties", Map(), JArray(Nil), Ascending),
          XRealField(XJValue, "default", Map(), JNothing, Ascending)
        )
      ),
      XCoproduct(
        Namespace("net.liftweb.json.xschema2"),
        "XOrder2",
        Map(),
        List(
          XReference("net.liftweb.json.xschema2.XOrderAscending2"),
          XReference("net.liftweb.json.xschema2.XOrderDescending2"),
          XReference("net.liftweb.json.xschema2.XOrderIgnore2")
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XOrderAscending2",
        Map(),
        List()
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XOrderDescending2",
        Map(),
        List()
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XOrderIgnore2",
        Map(),
        List()
      )
    ),
    Map(
    )
  )
}

}
