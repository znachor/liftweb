package net.liftweb.json.xschema {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.JsonParser._

import DefaultSerialization._

/*
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
}*/

object TestSchemas {
  val numberExpr = parse("""{ "Number": { "value": 0.0 } }""")
  val number     = parse("""{ "value": 0.0 }""")

  val AstNumericExprSchema = XRoot(
    List(
      XCoproduct(
        "Expr", "ast.numeric",
        Map(),
        List(
          XDefinitionRef("NumericSum", "ast.numeric"),
          XDefinitionRef("Sum", "ast.numeric"),
          XDefinitionRef("Product", "ast.numeric"),
          XDefinitionRef("Number", "ast.numeric")
        )
      ),
      XCoproduct(
        "MixedSum", "ast.numeric",
        Map(),
        List(
          XDefinitionRef("NumericSum", "ast.numeric"),
          XDefinitionRef("Sum", "ast.numeric")
        )
      ),
      XProduct(
        "NumericSum", "ast.numeric",
        Map(),
        List(
          XRealField("term1", Map(), XDefinitionRef("Number", "ast.numeric"), numberExpr, XOrderDescending),
          XRealField("term2", Map(), XDefinitionRef("Number", "ast.numeric"), numberExpr, XOrderDescending)
        )
      ),
      XProduct(
        "Sum", "ast.numeric",
        Map(),
        List(
          XRealField("term1", Map(), XDefinitionRef("Expr", "ast.numeric"), numberExpr, XOrderDescending),
          XRealField("term2", Map(), XDefinitionRef("Expr", "ast.numeric"), numberExpr, XOrderDescending)
        )
      ),
      XProduct(
        "Product", "ast.numeric",
        Map(),
        List(
          XRealField("term1", Map(), XDefinitionRef("Expr", "ast.numeric"), numberExpr, XOrderDescending),
          XRealField("term2", Map(), XDefinitionRef("Expr", "ast.numeric"), numberExpr, XOrderDescending)
        )
      ),
      XProduct(
        "Number", "ast.numeric",
        Map(),
        List(
          XRealField("value", Map(), XDouble, number, XOrderDescending)
        )
      )
    ),
    Map()
  )

  val DataSocialGenderSchema = XRoot(
    List(
      XCoproduct(
        "Gender", "data.social",
        Map(
          "xschema.doc" -> "This is the coproduct that includes male and female. The normal way to translate this into OOP is as a superclass/superinterface.",
          "scala.class.traits" -> "java.io.Serializable, java.lang.Cloneable",
          "scala.object.traits" -> "java.io.Serializable, java.lang.Cloneable"
        ),
        List(
          XDefinitionRef("Male", "data.social"),
          XDefinitionRef("Female", "data.social")
        )
      ),
      XProduct(
        "Male", "data.social",
        Map("scala.class.traits" -> "java.io.Serializable, java.lang.Cloneable"),
        List(
          XRealField("text", Map(), XString, JString("male"), XOrderDescending),
          XViewField("asFemale", Map(), XDefinitionRef("Female", "data.social"))
        )
      ),
      XProduct(
        "Female", "data.social",
        Map("scala.class.traits" -> "java.io.Serializable, java.lang.Cloneable"),
        List(
          XRealField("text", Map(), XString, JString("female"), XOrderAscending),
          XViewField("asMale", Map(), XDefinitionRef("Male", "data.social"))
        )
      ),
      XConstant(
        "DefaultFemale", "data.social",
        Map(),
        XDefinitionRef("Gender", "data.social"),
        JObject(
          JField("Female",
            JObject(
              JField("text", JString("female")) :: Nil
            )
          ) :: Nil
        )
      ),
      XConstant(
        "DefaultMale", "data.social",
        Map(),
        XDefinitionRef("Gender", "data.social"),
        JObject(
          JField("Male",
            JObject(
              JField("text", JString("male")) :: Nil
            )
          ) :: Nil
        )
      ),
      XProduct(
        "Morning", "data.social",
        Map(),
        List()
      ),
      XProduct(
        "Noon", "data.social",
        Map(),
        List()
      ),
      XProduct(
        "Night", "data.social",
        Map(),
        List()
      ),
      XCoproduct(
        "Time", "data.social",
        Map(),
        List(
          XDefinitionRef("Morning", "data.social"),
          XDefinitionRef("Noon", "data.social"),
          XDefinitionRef("Night", "data.social")
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
        "QualifiedName", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JNothing, XOrderAscending),
          XRealField("namespace", Map(), XString, JNothing, XOrderAscending)
        )
      ),
      XProduct(
        "XRoot", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("definitions", Map(), XList(XDefinitionRef("XDefinition", "net.liftweb.json.xschema")), JNothing, XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XString), JArray(Nil), XOrderAscending)
        )
      ),
      XCoproduct(
        "XSchema", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XDefinition", "net.liftweb.json.xschema"),
          XDefinitionRef("XReference", "net.liftweb.json.xschema"),
          XDefinitionRef("XField", "net.liftweb.json.xschema")
        )
      ),
      XCoproduct(
        "XReference", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XPrimitiveRef", "net.liftweb.json.xschema"),
          XDefinitionRef("XContainerRef", "net.liftweb.json.xschema"),
          XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema")
        )
      ),
      XCoproduct(
        "XPrimitiveRef", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XBoolean", "net.liftweb.json.xschema"),
          XDefinitionRef("XInt", "net.liftweb.json.xschema"),
          XDefinitionRef("XLong", "net.liftweb.json.xschema"),
          XDefinitionRef("XFloat", "net.liftweb.json.xschema"),
          XDefinitionRef("XDouble", "net.liftweb.json.xschema"),
          XDefinitionRef("XString", "net.liftweb.json.xschema"),
          XDefinitionRef("XJSON", "net.liftweb.json.xschema")
        )
      ),
      XCoproduct(
        "XContainerRef", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XCollection", "net.liftweb.json.xschema"),
          XDefinitionRef("XMap", "net.liftweb.json.xschema"),
          XDefinitionRef("XOptional", "net.liftweb.json.xschema"),
          XDefinitionRef("XTuple", "net.liftweb.json.xschema")
        )
      ),
      XProduct(
        "XDefinitionRef", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JNothing, XOrderAscending),
          XRealField("namespace", Map(), XString, JString(""), XOrderAscending),
          XViewField("qualifiedName", Map(), XDefinitionRef("QualifiedName", "net.liftweb.json.xschema"))
        )
      ),
      XProduct("XBoolean", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XInt", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XLong", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XFloat", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XDouble", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XString", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XJSON", "net.liftweb.json.xschema", Map(), List()),
      XCoproduct(
        "XCollection", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XList", "net.liftweb.json.xschema"),
          XDefinitionRef("XSet", "net.liftweb.json.xschema"),
          XDefinitionRef("XArray", "net.liftweb.json.xschema")
        )
      ),
      XProduct("XList", "net.liftweb.json.xschema", Map(), 
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending))
      ),
      XProduct("XSet", "net.liftweb.json.xschema", Map(), 
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending))
      ),
      XProduct("XArray", "net.liftweb.json.xschema", Map(), 
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending))
      ),
      XProduct("XMap", "net.liftweb.json.xschema", Map(), 
        List(
          XRealField("keyType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending),
          XRealField("valueType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending)
        )
      ),
      XProduct("XOptional", "net.liftweb.json.xschema", Map(), 
        List(XRealField("optionalType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending))
      ),
      XProduct("XTuple", "net.liftweb.json.xschema", Map(), 
        List(XRealField("types", Map(), XList(XDefinitionRef("XReference", "net.liftweb.json.xschema")), JNothing, XOrderAscending))
      ),
      XCoproduct(
        "XDefinition", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XProduct", "net.liftweb.json.xschema"),
          XDefinitionRef("XCoproduct", "net.liftweb.json.xschema"),
          XDefinitionRef("XConstant", "net.liftweb.json.xschema")
        )
      ),
      XCoproduct(
        "XField", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XRealField", "net.liftweb.json.xschema"),
          XDefinitionRef("XViewField", "net.liftweb.json.xschema"),
          XDefinitionRef("XConstantField", "net.liftweb.json.xschema")
        )
      ),
      XProduct(
        "XProduct", "net.liftweb.json.xschema",
        Map(
          "scala.class.traits" -> "net.liftweb.json.xschema.XProductBehavior"
        ),
        List(
          XRealField("name", Map(), XString, JNothing, XOrderAscending),
          XRealField("namespace", Map(), XString, JString(""), XOrderAscending),
          XViewField("qualifiedName", Map(), XDefinitionRef("QualifiedName", "net.liftweb.json.xschema")),
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema")),
          XRealField("properties", Map(), XMap(XString, XString), JArray(Nil), XOrderAscending),
          XRealField("terms", Map(), XList(XDefinitionRef("XField", "net.liftweb.json.xschema")), JObject(Nil), XOrderAscending)
        )
      ),
      XProduct(
        "XCoproduct", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JNothing, XOrderAscending),
          XRealField("namespace", Map(), XString, JString(""), XOrderAscending),
          XViewField("qualifiedName", Map(), XDefinitionRef("QualifiedName", "net.liftweb.json.xschema")),
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema")),
          XRealField("properties", Map(), XMap(XString, XString), JArray(Nil), XOrderAscending),
          XRealField("terms", Map(), XList(XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema")), JObject(Nil), XOrderAscending)
        )
      ),
      XProduct(
        "XConstant", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JNothing, XOrderAscending),
          XRealField("namespace", Map(), XString, JString(""), XOrderAscending),
          XViewField("qualifiedName", Map(), XDefinitionRef("QualifiedName", "net.liftweb.json.xschema")),
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema")),
          XRealField("properties", Map(), XMap(XString, XString), JArray(Nil), XOrderAscending),
          XRealField("default", Map(), XJSON, JNothing, XOrderAscending)
        )
      ),
      XProduct(
        "XRealField", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JNothing, XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XString), JArray(Nil), XOrderAscending),
          XRealField("fieldType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending),
          XRealField("default", Map(), XJSON, JNothing, XOrderAscending),
          XRealField("order", Map(), XDefinitionRef("XOrder", "net.liftweb.json.xschema"), JObject(JField("XOrderXOrderAscending", JObject(Nil)) :: Nil), XOrderAscending)
        )
      ),
      XProduct(
        "XViewField", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JNothing, XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XString), JArray(Nil), XOrderAscending),
          XRealField("fieldType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending)
        )
      ),
      XProduct(
        "XConstantField", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JNothing, XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XString), JArray(Nil), XOrderAscending),
          XRealField("fieldType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), JNothing, XOrderAscending),
          XRealField("default", Map(), XJSON, JNothing, XOrderAscending)
        )
      ),
      XCoproduct(
        "XOrder", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XOrderAscending", "net.liftweb.json.xschema"),
          XDefinitionRef("XOrderDescending", "net.liftweb.json.xschema"),
          XDefinitionRef("XOrderIgnore", "net.liftweb.json.xschema")
        )
      ),
      XProduct("XOrderAscending", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XOrderDescending", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XOrderIgnore", "net.liftweb.json.xschema", Map(), List())
    ),
    Map(
    )
  )
}

}







