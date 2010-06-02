package net.liftweb.json.xschema {

import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.JsonParser._

object TestSchemas {
  private def j(s: String) = parse(s)
  
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
        ),
        j(""" { "Male": { "text": "foo" } } """)
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
        ),
        j(""" { "Morning": {} } """)
      )
    ),
    List(
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
      )
    ),
    Map(
      "scala.imports" -> "net.liftweb.json.xschema.{SerializationImplicits => XSerializationImplicits, DefaultExtractors => XDefaultExtractors}, java.lang.reflect._"
    )
  )
  
  val XSchemaSchema = XRoot(
    List(
      XProduct(
        "XRoot", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("definitions", Map(), XList(XDefinitionRef("XDefinition", "net.liftweb.json.xschema")), j("""[]"""), XOrderAscending),
          XRealField("constants",   Map(), XList(XDefinitionRef("XConstant", "net.liftweb.json.xschema")), j("""[]"""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XString), j("""[]"""), XOrderAscending)
        )
      ),
      XCoproduct(
        "XSchema", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XDefinition", "net.liftweb.json.xschema"),
          XDefinitionRef("XReference",  "net.liftweb.json.xschema"),
          XDefinitionRef("XField",      "net.liftweb.json.xschema"),
          XDefinitionRef("XConstant",   "net.liftweb.json.xschema")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XReference", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XPrimitiveRef",  "net.liftweb.json.xschema"),
          XDefinitionRef("XContainerRef",  "net.liftweb.json.xschema"),
          XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema"),
          XDefinitionRef("XUnionRef",      "net.liftweb.json.xschema")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XPrimitiveRef", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XBoolean", "net.liftweb.json.xschema"),
          XDefinitionRef("XInt",     "net.liftweb.json.xschema"),
          XDefinitionRef("XLong",    "net.liftweb.json.xschema"),
          XDefinitionRef("XFloat",   "net.liftweb.json.xschema"),
          XDefinitionRef("XDouble",  "net.liftweb.json.xschema"),
          XDefinitionRef("XString",  "net.liftweb.json.xschema"),
          XDefinitionRef("XJSON",    "net.liftweb.json.xschema")
        ),
        j("""{ "XString": {} } """)
      ),
      XCoproduct(
        "XContainerRef", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XCollection", "net.liftweb.json.xschema"),
          XDefinitionRef("XMap",        "net.liftweb.json.xschema"),
          XDefinitionRef("XOptional",   "net.liftweb.json.xschema"),
          XDefinitionRef("XTuple",      "net.liftweb.json.xschema")
        ),
        j(""" { "XList": { "elementType": { "XString": {} } } } """)
      ),
      XProduct(
        "XDefinitionRef", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name", Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace", Map(), XString, JString(""), XOrderAscending)
        )
      ),
      XProduct(
        "XUnionRef", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("terms", Map(), XList(XDefinitionRef("XReference", "net.liftweb.json.xschema")), j("""[]"""), XOrderAscending)
        )
      ),
      XProduct("XBoolean", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XInt",     "net.liftweb.json.xschema", Map(), List()),
      XProduct("XLong",    "net.liftweb.json.xschema", Map(), List()),
      XProduct("XFloat",   "net.liftweb.json.xschema", Map(), List()),
      XProduct("XDouble",  "net.liftweb.json.xschema", Map(), List()),
      XProduct("XString",  "net.liftweb.json.xschema", Map(), List()),
      XProduct("XJSON",    "net.liftweb.json.xschema", Map(), List()),
      XCoproduct(
        "XCollection", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XList", "net.liftweb.json.xschema"),
          XDefinitionRef("XSet", "net.liftweb.json.xschema"),
          XDefinitionRef("XArray", "net.liftweb.json.xschema")
        ),
        j(""" { "XList": { "elementType": { "XString": {} } } } """)
      ),
      XProduct("XList", "net.liftweb.json.xschema", Map(), 
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XSet", "net.liftweb.json.xschema", Map(), 
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XArray", "net.liftweb.json.xschema", Map(), 
        List(XRealField("elementType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XMap", "net.liftweb.json.xschema", Map(), 
        List(
          XRealField("keyType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending),
          XRealField("valueType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending)
        )
      ),
      XProduct("XOptional", "net.liftweb.json.xschema", Map(), 
        List(XRealField("optionalType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending))
      ),
      XProduct("XTuple", "net.liftweb.json.xschema", Map(), 
        List(XRealField("types", Map(), XList(XDefinitionRef("XReference", "net.liftweb.json.xschema")), j("""[]"""), XOrderAscending))
      ),
      XCoproduct(
        "XDefinition", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XProduct", "net.liftweb.json.xschema"),
          XDefinitionRef("XCoproduct", "net.liftweb.json.xschema")
        ),
        j(""" { "XProduct": {} } """)
      ),
      XCoproduct(
        "XField", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XRealField", "net.liftweb.json.xschema"),
          XDefinitionRef("XViewField", "net.liftweb.json.xschema"),
          XDefinitionRef("XConstantField", "net.liftweb.json.xschema")
        ),
        j(""" { "XRealField": {} } """)
      ),
      XProduct(
        "XProduct", "net.liftweb.json.xschema",
        Map(
          "scala.class.traits" -> "net.liftweb.json.xschema.XProductBehavior"
        ),
        List(
          XRealField("name",        Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",   Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XString), j("""[]"""), XOrderAscending),
          XRealField("terms",       Map(), XList(XDefinitionRef("XField", "net.liftweb.json.xschema")), j("""[]"""), XOrderAscending),
          
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema"))
        )
      ),
      XProduct(
        "XCoproduct", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name",        Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",   Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",  Map(), XMap(XString, XString), j("""[]"""), XOrderAscending),
          XRealField("terms",       Map(), XList(XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema")), j("""[]"""), XOrderAscending),
          XRealField("default",     Map(), XJSON, JNothing, XOrderAscending),
          
          XViewField("referenceTo", Map(), XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema"))
        )
      ),
      XProduct(
        "XConstant", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name",         Map(), XString, JString(""), XOrderAscending),
          XRealField("namespace",    Map(), XString, JString(""), XOrderAscending),
          XRealField("properties",   Map(), XMap(XString, XString), j("""[]"""), XOrderAscending),
          XRealField("constantType", Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",      Map(), XJSON, JString(""), XOrderAscending),
          
          XViewField("referenceTo",  Map(), XDefinitionRef("XDefinitionRef", "net.liftweb.json.xschema"))
        )
      ),
      XProduct(
        "XRealField", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XString), JArray(Nil), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",    Map(), XJSON, JString(""), XOrderAscending),
          XRealField("order",      Map(), XDefinitionRef("XOrder", "net.liftweb.json.xschema"), j(""" { "XOrderAscending": {} } """), XOrderAscending)
        )
      ),
      XProduct(
        "XViewField", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XString), j("""[]"""), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XDefinitionRef": { "name": "", "namespace": "" } }  """), XOrderAscending)
        )
      ),
      XProduct(
        "XConstantField", "net.liftweb.json.xschema",
        Map(),
        List(
          XRealField("name",       Map(), XString, JString(""), XOrderAscending),
          XRealField("properties", Map(), XMap(XString, XString), j("""[]"""), XOrderAscending),
          XRealField("fieldType",  Map(), XDefinitionRef("XReference", "net.liftweb.json.xschema"), j(""" { "XString": {} }  """), XOrderAscending),
          XRealField("default",    Map(), XJSON, JString(""), XOrderAscending)
        )
      ),
      XCoproduct(
        "XOrder", "net.liftweb.json.xschema",
        Map(),
        List(
          XDefinitionRef("XOrderAscending",  "net.liftweb.json.xschema"),
          XDefinitionRef("XOrderDescending", "net.liftweb.json.xschema"),
          XDefinitionRef("XOrderIgnore",     "net.liftweb.json.xschema")
        ),
        j(""" { "XOrderAscending": {} } """)
      ),
      XProduct("XOrderAscending",  "net.liftweb.json.xschema", Map(), List()),
      XProduct("XOrderDescending", "net.liftweb.json.xschema", Map(), List()),
      XProduct("XOrderIgnore",     "net.liftweb.json.xschema", Map(), List())
    ),
    Nil,
    Map(
    )
  )
}

}







