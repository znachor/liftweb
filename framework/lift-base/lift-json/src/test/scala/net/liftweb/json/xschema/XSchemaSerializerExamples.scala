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
      XCoproduct(
        Namespace("net.liftweb.json.xschema2"),
        "XReference2",
        Map(),
        List(
          XReference("net.liftweb.json.xschema2.XPrimitiveRef2"),
          XReference("net.liftweb.json.xschema2.XDefinitionRef2")
        )
      ),
      XCoproduct(
        Namespace("net.liftweb.json.xschema2"),
        "XPrimitiveRef2",
        Map(),
        List(
          XReference("net.liftweb.json.xschema2.XBoolean2"),
          XReference("net.liftweb.json.xschema2.XInt2"),
          XReference("net.liftweb.json.xschema2.XLong2"),
          XReference("net.liftweb.json.xschema2.XFloat2"),
          XReference("net.liftweb.json.xschema2.XDouble2"),
          XReference("net.liftweb.json.xschema2.XString2"),
          XReference("net.liftweb.json.xschema2.XJSON2"),
          XReference("net.liftweb.json.xschema2.XCollection2"),
          XReference("net.liftweb.json.xschema2.XMap2"),
          XReference("net.liftweb.json.xschema2.XOptional2"),
          XReference("net.liftweb.json.xschema2.XTuple2")
        )
      ),
      XProduct(
        Namespace("net.liftweb.json.xschema2"),
        "XDefinitionRef2",
        Map(),
        List(
          XRealField(XString, "name", Map(), JNothing, Ascending),
          XRealField(XString, "namespace", Map(), JString(""), Ascending),
          XViewField(XReference("net.liftweb.json.xschema2.QualifiedName2"), "qualifiedName", Map())
        )
      ),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XBoolean2", Map(), List()),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XInt2", Map(), List()),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XLong2", Map(), List()),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XFloat2", Map(), List()),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XDouble2", Map(), List()),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XString2", Map(), List()),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XJSON2", Map(), List()),
      XCoproduct(
        Namespace("net.liftweb.json.xschema2"),
        "XCollection2",
        Map(),
        List(
          XReference("net.liftweb.json.xschema2.XList2"),
          XReference("net.liftweb.json.xschema2.XSet2"),
          XReference("net.liftweb.json.xschema2.XArray2")
        )
      ),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XList2", Map(), 
        List(XRealField(XReference("net.liftweb.json.xschema2.XReference2"), "elementType", Map(), JNothing, Ascending))
      ),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XSet2", Map(), 
        List(XRealField(XReference("net.liftweb.json.xschema2.XReference2"), "elementType", Map(), JNothing, Ascending))
      ),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XArray2", Map(), 
        List(XRealField(XReference("net.liftweb.json.xschema2.XReference2"), "elementType", Map(), JNothing, Ascending))
      ),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XMap2", Map(), 
        List(
          XRealField(XReference("net.liftweb.json.xschema2.XReference2"), "keyType", Map(), JNothing, Ascending),
          XRealField(XReference("net.liftweb.json.xschema2.XReference2"), "valueType", Map(), JNothing, Ascending)
        )
      ),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XOptional2", Map(), 
        List(XRealField(XReference("net.liftweb.json.xschema2.XReference2"), "optionalType", Map(), JNothing, Ascending))
      ),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XTuple2", Map(), 
        List(XRealField(XCollection(XReference("net.liftweb.json.xschema2.XReference2"), XList), "types", Map(), JNothing, Ascending))
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
      XProduct(Namespace("net.liftweb.json.xschema2"), "XOrderAscending2", Map(), List()),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XOrderDescending2", Map(), List()),
      XProduct(Namespace("net.liftweb.json.xschema2"), "XOrderIgnore2", Map(), List())
    ),
    Map(
    )
  )
}

}







package net.liftweb.json.xschema2 {
  import net.liftweb.json.JsonParser._
  import net.liftweb.json.JsonAST._
  import net.liftweb.json.xschema.{Extractor, Decomposer, SerializationImplicits, DefaultExtractors, ExtractionHelpers, DefaultDecomposers, DecomposerHelpers, DefaultOrderings}
  import net.liftweb.json.xschema.Serialization._
  import net.liftweb.json.xschema.XSchemaAST
  
  
  
  trait Orderings {
    implicit def XSchema2ToOrderedXSchema2(inner: XSchema2) = OrderedXSchema2(inner)
    implicit def XReference2ToOrderedXReference2(inner: XReference2) = OrderedXReference2(inner)
    implicit def XPrimitiveRef2ToOrderedXPrimitiveRef2(inner: XPrimitiveRef2) = OrderedXPrimitiveRef2(inner)
    implicit def XCollection2ToOrderedXCollection2(inner: XCollection2) = OrderedXCollection2(inner)
    implicit def XDefinition2ToOrderedXDefinition2(inner: XDefinition2) = OrderedXDefinition2(inner)
    implicit def XField2ToOrderedXField2(inner: XField2) = OrderedXField2(inner)
    implicit def XOrder2ToOrderedXOrder2(inner: XOrder2) = OrderedXOrder2(inner)
    
    case class OrderedXSchema2(inner: net.liftweb.json.xschema2.XSchema2) extends Ordered[net.liftweb.json.xschema2.XSchema2] {
      def compare(that: net.liftweb.json.xschema2.XSchema2): Int = {
        if (inner == that) 0
        else inner match {
          case x: net.liftweb.json.xschema2.XDefinition2 => that match {
            case y: net.liftweb.json.xschema2.XDefinition2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XReference2 => -1
          }
          case x: net.liftweb.json.xschema2.XReference2 => that match {
            case y: net.liftweb.json.xschema2.XDefinition2 => 1
            case y: net.liftweb.json.xschema2.XReference2 => x.compare(y)
          }
        }
      }
    }
    
    case class OrderedXReference2(inner: net.liftweb.json.xschema2.XReference2) extends Ordered[net.liftweb.json.xschema2.XReference2] {
      def compare(that: net.liftweb.json.xschema2.XReference2): Int = {
        if (inner == that) 0
        else inner match {
          case x: net.liftweb.json.xschema2.XPrimitiveRef2 => that match {
            case y: net.liftweb.json.xschema2.XPrimitiveRef2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XDefinitionRef2 => -1
          }
          case x: net.liftweb.json.xschema2.XDefinitionRef2 => that match {
            case y: net.liftweb.json.xschema2.XPrimitiveRef2 => 1
            case y: net.liftweb.json.xschema2.XDefinitionRef2 => x.compare(y)
          }
        }
      }
    }
    
    case class OrderedXPrimitiveRef2(inner: net.liftweb.json.xschema2.XPrimitiveRef2) extends Ordered[net.liftweb.json.xschema2.XPrimitiveRef2] {
      def compare(that: net.liftweb.json.xschema2.XPrimitiveRef2): Int = {
        if (inner == that) 0
        else inner match {
          case x: net.liftweb.json.xschema2.XBoolean2.type => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XInt2.type => -1
            case y: net.liftweb.json.xschema2.XLong2.type => -1
            case y: net.liftweb.json.xschema2.XFloat2.type => -1
            case y: net.liftweb.json.xschema2.XDouble2.type => -1
            case y: net.liftweb.json.xschema2.XString2.type => -1
            case y: net.liftweb.json.xschema2.XJSON2.type => -1
            case y: net.liftweb.json.xschema2.XCollection2 => -1
            case y: net.liftweb.json.xschema2.XMap2 => -1
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XInt2.type => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XLong2.type => -1
            case y: net.liftweb.json.xschema2.XFloat2.type => -1
            case y: net.liftweb.json.xschema2.XDouble2.type => -1
            case y: net.liftweb.json.xschema2.XString2.type => -1
            case y: net.liftweb.json.xschema2.XJSON2.type => -1
            case y: net.liftweb.json.xschema2.XCollection2 => -1
            case y: net.liftweb.json.xschema2.XMap2 => -1
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XLong2.type => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XFloat2.type => -1
            case y: net.liftweb.json.xschema2.XDouble2.type => -1
            case y: net.liftweb.json.xschema2.XString2.type => -1
            case y: net.liftweb.json.xschema2.XJSON2.type => -1
            case y: net.liftweb.json.xschema2.XCollection2 => -1
            case y: net.liftweb.json.xschema2.XMap2 => -1
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XFloat2.type => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => 1
            case y: net.liftweb.json.xschema2.XFloat2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XDouble2.type => -1
            case y: net.liftweb.json.xschema2.XString2.type => -1
            case y: net.liftweb.json.xschema2.XJSON2.type => -1
            case y: net.liftweb.json.xschema2.XCollection2 => -1
            case y: net.liftweb.json.xschema2.XMap2 => -1
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XDouble2.type => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => 1
            case y: net.liftweb.json.xschema2.XFloat2.type => 1
            case y: net.liftweb.json.xschema2.XDouble2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XString2.type => -1
            case y: net.liftweb.json.xschema2.XJSON2.type => -1
            case y: net.liftweb.json.xschema2.XCollection2 => -1
            case y: net.liftweb.json.xschema2.XMap2 => -1
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XString2.type => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => 1
            case y: net.liftweb.json.xschema2.XFloat2.type => 1
            case y: net.liftweb.json.xschema2.XDouble2.type => 1
            case y: net.liftweb.json.xschema2.XString2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XJSON2.type => -1
            case y: net.liftweb.json.xschema2.XCollection2 => -1
            case y: net.liftweb.json.xschema2.XMap2 => -1
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XJSON2.type => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => 1
            case y: net.liftweb.json.xschema2.XFloat2.type => 1
            case y: net.liftweb.json.xschema2.XDouble2.type => 1
            case y: net.liftweb.json.xschema2.XString2.type => 1
            case y: net.liftweb.json.xschema2.XJSON2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XCollection2 => -1
            case y: net.liftweb.json.xschema2.XMap2 => -1
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XCollection2 => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => 1
            case y: net.liftweb.json.xschema2.XFloat2.type => 1
            case y: net.liftweb.json.xschema2.XDouble2.type => 1
            case y: net.liftweb.json.xschema2.XString2.type => 1
            case y: net.liftweb.json.xschema2.XJSON2.type => 1
            case y: net.liftweb.json.xschema2.XCollection2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XMap2 => -1
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XMap2 => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => 1
            case y: net.liftweb.json.xschema2.XFloat2.type => 1
            case y: net.liftweb.json.xschema2.XDouble2.type => 1
            case y: net.liftweb.json.xschema2.XString2.type => 1
            case y: net.liftweb.json.xschema2.XJSON2.type => 1
            case y: net.liftweb.json.xschema2.XCollection2 => 1
            case y: net.liftweb.json.xschema2.XMap2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XOptional2 => -1
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XOptional2 => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => 1
            case y: net.liftweb.json.xschema2.XFloat2.type => 1
            case y: net.liftweb.json.xschema2.XDouble2.type => 1
            case y: net.liftweb.json.xschema2.XString2.type => 1
            case y: net.liftweb.json.xschema2.XJSON2.type => 1
            case y: net.liftweb.json.xschema2.XCollection2 => 1
            case y: net.liftweb.json.xschema2.XMap2 => 1
            case y: net.liftweb.json.xschema2.XOptional2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XTuple2 => -1
          }
          case x: net.liftweb.json.xschema2.XTuple2 => that match {
            case y: net.liftweb.json.xschema2.XBoolean2.type => 1
            case y: net.liftweb.json.xschema2.XInt2.type => 1
            case y: net.liftweb.json.xschema2.XLong2.type => 1
            case y: net.liftweb.json.xschema2.XFloat2.type => 1
            case y: net.liftweb.json.xschema2.XDouble2.type => 1
            case y: net.liftweb.json.xschema2.XString2.type => 1
            case y: net.liftweb.json.xschema2.XJSON2.type => 1
            case y: net.liftweb.json.xschema2.XCollection2 => 1
            case y: net.liftweb.json.xschema2.XMap2 => 1
            case y: net.liftweb.json.xschema2.XOptional2 => 1
            case y: net.liftweb.json.xschema2.XTuple2 => x.compare(y)
          }
        }
      }
    }
    
    case class OrderedXCollection2(inner: net.liftweb.json.xschema2.XCollection2) extends Ordered[net.liftweb.json.xschema2.XCollection2] {
      def compare(that: net.liftweb.json.xschema2.XCollection2): Int = {
        if (inner == that) 0
        else inner match {
          case x: net.liftweb.json.xschema2.XList2 => that match {
            case y: net.liftweb.json.xschema2.XList2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XSet2 => -1
            case y: net.liftweb.json.xschema2.XArray2 => -1
          }
          case x: net.liftweb.json.xschema2.XSet2 => that match {
            case y: net.liftweb.json.xschema2.XList2 => 1
            case y: net.liftweb.json.xschema2.XSet2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XArray2 => -1
          }
          case x: net.liftweb.json.xschema2.XArray2 => that match {
            case y: net.liftweb.json.xschema2.XList2 => 1
            case y: net.liftweb.json.xschema2.XSet2 => 1
            case y: net.liftweb.json.xschema2.XArray2 => x.compare(y)
          }
        }
      }
    }
    
    case class OrderedXDefinition2(inner: net.liftweb.json.xschema2.XDefinition2) extends Ordered[net.liftweb.json.xschema2.XDefinition2] {
      def compare(that: net.liftweb.json.xschema2.XDefinition2): Int = {
        if (inner == that) 0
        else inner match {
          case x: net.liftweb.json.xschema2.XField2 => that match {
            case y: net.liftweb.json.xschema2.XField2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XProduct2 => -1
            case y: net.liftweb.json.xschema2.XCoproduct2 => -1
            case y: net.liftweb.json.xschema2.XConstant2 => -1
          }
          case x: net.liftweb.json.xschema2.XProduct2 => that match {
            case y: net.liftweb.json.xschema2.XField2 => 1
            case y: net.liftweb.json.xschema2.XProduct2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XCoproduct2 => -1
            case y: net.liftweb.json.xschema2.XConstant2 => -1
          }
          case x: net.liftweb.json.xschema2.XCoproduct2 => that match {
            case y: net.liftweb.json.xschema2.XField2 => 1
            case y: net.liftweb.json.xschema2.XProduct2 => 1
            case y: net.liftweb.json.xschema2.XCoproduct2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XConstant2 => -1
          }
          case x: net.liftweb.json.xschema2.XConstant2 => that match {
            case y: net.liftweb.json.xschema2.XField2 => 1
            case y: net.liftweb.json.xschema2.XProduct2 => 1
            case y: net.liftweb.json.xschema2.XCoproduct2 => 1
            case y: net.liftweb.json.xschema2.XConstant2 => x.compare(y)
          }
        }
      }
    }
    
    case class OrderedXField2(inner: net.liftweb.json.xschema2.XField2) extends Ordered[net.liftweb.json.xschema2.XField2] {
      def compare(that: net.liftweb.json.xschema2.XField2): Int = {
        if (inner == that) 0
        else inner match {
          case x: net.liftweb.json.xschema2.XRealField2 => that match {
            case y: net.liftweb.json.xschema2.XRealField2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XViewField2 => -1
            case y: net.liftweb.json.xschema2.XConstantField2 => -1
          }
          case x: net.liftweb.json.xschema2.XViewField2 => that match {
            case y: net.liftweb.json.xschema2.XRealField2 => 1
            case y: net.liftweb.json.xschema2.XViewField2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XConstantField2 => -1
          }
          case x: net.liftweb.json.xschema2.XConstantField2 => that match {
            case y: net.liftweb.json.xschema2.XRealField2 => 1
            case y: net.liftweb.json.xschema2.XViewField2 => 1
            case y: net.liftweb.json.xschema2.XConstantField2 => x.compare(y)
          }
        }
      }
    }
    
    case class OrderedXOrder2(inner: net.liftweb.json.xschema2.XOrder2) extends Ordered[net.liftweb.json.xschema2.XOrder2] {
      def compare(that: net.liftweb.json.xschema2.XOrder2): Int = {
        if (inner == that) 0
        else inner match {
          case x: net.liftweb.json.xschema2.XOrderAscending2.type => that match {
            case y: net.liftweb.json.xschema2.XOrderAscending2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XOrderDescending2.type => -1
            case y: net.liftweb.json.xschema2.XOrderIgnore2.type => -1
          }
          case x: net.liftweb.json.xschema2.XOrderDescending2.type => that match {
            case y: net.liftweb.json.xschema2.XOrderAscending2.type => 1
            case y: net.liftweb.json.xschema2.XOrderDescending2.type => x.compare(y)
            case y: net.liftweb.json.xschema2.XOrderIgnore2.type => -1
          }
          case x: net.liftweb.json.xschema2.XOrderIgnore2.type => that match {
            case y: net.liftweb.json.xschema2.XOrderAscending2.type => 1
            case y: net.liftweb.json.xschema2.XOrderDescending2.type => 1
            case y: net.liftweb.json.xschema2.XOrderIgnore2.type => x.compare(y)
          }
        }
      }
    }
  }
  object Orderings extends Orderings
  
  sealed trait XSchema2 extends Product {
    
  }
  
  sealed trait XReference2 extends Product with net.liftweb.json.xschema2.XSchema2 {
    
  }
  
  sealed trait XPrimitiveRef2 extends Product with net.liftweb.json.xschema2.XReference2 {
    
  }
  
  sealed trait XCollection2 extends Product with net.liftweb.json.xschema2.XPrimitiveRef2 {
    def elementType: net.liftweb.json.xschema2.XReference2
  }
  
  sealed trait XDefinition2 extends Product with net.liftweb.json.xschema2.XSchema2 {
    def name: String
    def properties: Map[String, String]
  }
  
  sealed trait XField2 extends Product with net.liftweb.json.xschema2.XDefinition2 {
    def name: String
    def properties: Map[String, String]
  }
  
  sealed trait XOrder2 extends Product {
    
  }
  
  case class QualifiedName2(name: String, namespace: String) extends Ordered[net.liftweb.json.xschema2.QualifiedName2] {
    def compare(that: net.liftweb.json.xschema2.QualifiedName2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      c = this.namespace.compare(that.namespace)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XDefinitionRef2(name: String, namespace: String) extends Ordered[net.liftweb.json.xschema2.XDefinitionRef2] with net.liftweb.json.xschema2.XReference2 {
    def compare(that: net.liftweb.json.xschema2.XDefinitionRef2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      c = this.namespace.compare(that.namespace)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    def qualifiedName: net.liftweb.json.xschema2.QualifiedName2 = net.liftweb.json.xschema2.QualifiedName2(name, namespace)
  }
  
  case object XBoolean2 extends net.liftweb.json.xschema2.XPrimitiveRef2 
  
  case object XInt2 extends net.liftweb.json.xschema2.XPrimitiveRef2 
  
  case object XLong2 extends net.liftweb.json.xschema2.XPrimitiveRef2 
  
  case object XFloat2 extends net.liftweb.json.xschema2.XPrimitiveRef2 
  
  case object XDouble2 extends net.liftweb.json.xschema2.XPrimitiveRef2 
  
  case object XString2 extends net.liftweb.json.xschema2.XPrimitiveRef2 
  
  case object XJSON2 extends net.liftweb.json.xschema2.XPrimitiveRef2 
  
  case class XList2(elementType: net.liftweb.json.xschema2.XReference2) extends Ordered[net.liftweb.json.xschema2.XList2] with net.liftweb.json.xschema2.XCollection2 {
    def compare(that: net.liftweb.json.xschema2.XList2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.elementType.compare(that.elementType)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XSet2(elementType: net.liftweb.json.xschema2.XReference2) extends Ordered[net.liftweb.json.xschema2.XSet2] with net.liftweb.json.xschema2.XCollection2 {
    def compare(that: net.liftweb.json.xschema2.XSet2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.elementType.compare(that.elementType)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XArray2(elementType: net.liftweb.json.xschema2.XReference2) extends Ordered[net.liftweb.json.xschema2.XArray2] with net.liftweb.json.xschema2.XCollection2 {
    def compare(that: net.liftweb.json.xschema2.XArray2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.elementType.compare(that.elementType)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XMap2(keyType: net.liftweb.json.xschema2.XReference2, valueType: net.liftweb.json.xschema2.XReference2) extends Ordered[net.liftweb.json.xschema2.XMap2] with net.liftweb.json.xschema2.XPrimitiveRef2 {
    def compare(that: net.liftweb.json.xschema2.XMap2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.keyType.compare(that.keyType)
      if (c != 0) return c * 1
      
      c = this.valueType.compare(that.valueType)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XOptional2(optionalType: net.liftweb.json.xschema2.XReference2) extends Ordered[net.liftweb.json.xschema2.XOptional2] with net.liftweb.json.xschema2.XPrimitiveRef2 {
    def compare(that: net.liftweb.json.xschema2.XOptional2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.optionalType.compare(that.optionalType)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XTuple2(types: List[net.liftweb.json.xschema2.XReference2]) extends Ordered[net.liftweb.json.xschema2.XTuple2] with net.liftweb.json.xschema2.XPrimitiveRef2 {
    def compare(that: net.liftweb.json.xschema2.XTuple2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XProduct2(name: String, namespace: String, properties: Map[String, String], terms: List[net.liftweb.json.xschema2.XReference2]) extends Ordered[net.liftweb.json.xschema2.XProduct2] with net.liftweb.json.xschema2.XDefinition2 {
    def compare(that: net.liftweb.json.xschema2.XProduct2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      c = this.namespace.compare(that.namespace)
      if (c != 0) return c * 1
      
      
      
      return this.hashCode - that.hashCode
    }
    def qualifiedName: net.liftweb.json.xschema2.QualifiedName2 = net.liftweb.json.xschema2.QualifiedName2(name, namespace)
  }
  
  case class XCoproduct2(name: String, namespace: String, properties: Map[String, String], terms: List[net.liftweb.json.xschema2.XReference2]) extends Ordered[net.liftweb.json.xschema2.XCoproduct2] with net.liftweb.json.xschema2.XDefinition2 {
    def compare(that: net.liftweb.json.xschema2.XCoproduct2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      c = this.namespace.compare(that.namespace)
      if (c != 0) return c * 1
      
      
      
      return this.hashCode - that.hashCode
    }
    def qualifiedName: net.liftweb.json.xschema2.QualifiedName2 = net.liftweb.json.xschema2.QualifiedName2(name, namespace)
  }
  
  case class XConstant2(name: String, namespace: String, properties: Map[String, String], default: net.liftweb.json.JsonAST.JValue) extends Ordered[net.liftweb.json.xschema2.XConstant2] with net.liftweb.json.xschema2.XDefinition2 {
    def compare(that: net.liftweb.json.xschema2.XConstant2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      c = this.namespace.compare(that.namespace)
      if (c != 0) return c * 1
      
      
      c = this.default.compare(that.default)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    def qualifiedName: net.liftweb.json.xschema2.QualifiedName2 = net.liftweb.json.xschema2.QualifiedName2(name, namespace)
  }
  
  case class XRealField2(name: String, properties: Map[String, String], default: net.liftweb.json.JsonAST.JValue, order: net.liftweb.json.xschema2.XOrder2) extends Ordered[net.liftweb.json.xschema2.XRealField2] with net.liftweb.json.xschema2.XField2 {
    def compare(that: net.liftweb.json.xschema2.XRealField2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      
      c = this.default.compare(that.default)
      if (c != 0) return c * 1
      
      c = this.order.compare(that.order)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XViewField2(name: String, properties: Map[String, String]) extends Ordered[net.liftweb.json.xschema2.XViewField2] with net.liftweb.json.xschema2.XField2 {
    def compare(that: net.liftweb.json.xschema2.XViewField2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XConstantField2(name: String, properties: Map[String, String], default: net.liftweb.json.JsonAST.JValue) extends Ordered[net.liftweb.json.xschema2.XConstantField2] with net.liftweb.json.xschema2.XField2 {
    def compare(that: net.liftweb.json.xschema2.XConstantField2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      
      c = this.default.compare(that.default)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case object XOrderAscending2 extends net.liftweb.json.xschema2.XOrder2 
  
  case object XOrderDescending2 extends net.liftweb.json.xschema2.XOrder2 
  
  case object XOrderIgnore2 extends net.liftweb.json.xschema2.XOrder2 
  
  trait Extractors extends DefaultExtractors with ExtractionHelpers {
    private lazy val XSchema2ExtractorFunction: PartialFunction[JField, net.liftweb.json.xschema2.XSchema2] = List[PartialFunction[JField, net.liftweb.json.xschema2.XSchema2]](
      { case JField("XDefinition2", value) => net.liftweb.json.xschema2.Serialization.XDefinition2Extractor.extract(value) },
      { case JField("XReference2", value) => net.liftweb.json.xschema2.Serialization.XReference2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XSchema2Extractor: Extractor[net.liftweb.json.xschema2.XSchema2] = new Extractor[net.liftweb.json.xschema2.XSchema2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XSchema2 = {
        (jvalue --> classOf[JObject]).obj.filter(XSchema2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XSchema2ExtractorFunction(field)
          case Nil => error("Expected to find net.liftweb.json.xschema2.XSchema2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XReference2ExtractorFunction: PartialFunction[JField, net.liftweb.json.xschema2.XReference2] = List[PartialFunction[JField, net.liftweb.json.xschema2.XReference2]](
      { case JField("XPrimitiveRef2", value) => net.liftweb.json.xschema2.Serialization.XPrimitiveRef2Extractor.extract(value) },
      { case JField("XDefinitionRef2", value) => net.liftweb.json.xschema2.Serialization.XDefinitionRef2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XReference2Extractor: Extractor[net.liftweb.json.xschema2.XReference2] = new Extractor[net.liftweb.json.xschema2.XReference2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XReference2 = {
        (jvalue --> classOf[JObject]).obj.filter(XReference2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XReference2ExtractorFunction(field)
          case Nil => error("Expected to find net.liftweb.json.xschema2.XReference2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XPrimitiveRef2ExtractorFunction: PartialFunction[JField, net.liftweb.json.xschema2.XPrimitiveRef2] = List[PartialFunction[JField, net.liftweb.json.xschema2.XPrimitiveRef2]](
      { case JField("XBoolean2", value) => net.liftweb.json.xschema2.Serialization.XBoolean2Extractor.extract(value) },
      { case JField("XInt2", value) => net.liftweb.json.xschema2.Serialization.XInt2Extractor.extract(value) },
      { case JField("XLong2", value) => net.liftweb.json.xschema2.Serialization.XLong2Extractor.extract(value) },
      { case JField("XFloat2", value) => net.liftweb.json.xschema2.Serialization.XFloat2Extractor.extract(value) },
      { case JField("XDouble2", value) => net.liftweb.json.xschema2.Serialization.XDouble2Extractor.extract(value) },
      { case JField("XString2", value) => net.liftweb.json.xschema2.Serialization.XString2Extractor.extract(value) },
      { case JField("XJSON2", value) => net.liftweb.json.xschema2.Serialization.XJSON2Extractor.extract(value) },
      { case JField("XCollection2", value) => net.liftweb.json.xschema2.Serialization.XCollection2Extractor.extract(value) },
      { case JField("XMap2", value) => net.liftweb.json.xschema2.Serialization.XMap2Extractor.extract(value) },
      { case JField("XOptional2", value) => net.liftweb.json.xschema2.Serialization.XOptional2Extractor.extract(value) },
      { case JField("XTuple2", value) => net.liftweb.json.xschema2.Serialization.XTuple2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XPrimitiveRef2Extractor: Extractor[net.liftweb.json.xschema2.XPrimitiveRef2] = new Extractor[net.liftweb.json.xschema2.XPrimitiveRef2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XPrimitiveRef2 = {
        (jvalue --> classOf[JObject]).obj.filter(XPrimitiveRef2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XPrimitiveRef2ExtractorFunction(field)
          case Nil => error("Expected to find net.liftweb.json.xschema2.XPrimitiveRef2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XCollection2ExtractorFunction: PartialFunction[JField, net.liftweb.json.xschema2.XCollection2] = List[PartialFunction[JField, net.liftweb.json.xschema2.XCollection2]](
      { case JField("XList2", value) => net.liftweb.json.xschema2.Serialization.XList2Extractor.extract(value) },
      { case JField("XSet2", value) => net.liftweb.json.xschema2.Serialization.XSet2Extractor.extract(value) },
      { case JField("XArray2", value) => net.liftweb.json.xschema2.Serialization.XArray2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XCollection2Extractor: Extractor[net.liftweb.json.xschema2.XCollection2] = new Extractor[net.liftweb.json.xschema2.XCollection2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XCollection2 = {
        (jvalue --> classOf[JObject]).obj.filter(XCollection2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XCollection2ExtractorFunction(field)
          case Nil => error("Expected to find net.liftweb.json.xschema2.XCollection2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XDefinition2ExtractorFunction: PartialFunction[JField, net.liftweb.json.xschema2.XDefinition2] = List[PartialFunction[JField, net.liftweb.json.xschema2.XDefinition2]](
      { case JField("XField2", value) => net.liftweb.json.xschema2.Serialization.XField2Extractor.extract(value) },
      { case JField("XProduct2", value) => net.liftweb.json.xschema2.Serialization.XProduct2Extractor.extract(value) },
      { case JField("XCoproduct2", value) => net.liftweb.json.xschema2.Serialization.XCoproduct2Extractor.extract(value) },
      { case JField("XConstant2", value) => net.liftweb.json.xschema2.Serialization.XConstant2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XDefinition2Extractor: Extractor[net.liftweb.json.xschema2.XDefinition2] = new Extractor[net.liftweb.json.xschema2.XDefinition2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XDefinition2 = {
        (jvalue --> classOf[JObject]).obj.filter(XDefinition2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XDefinition2ExtractorFunction(field)
          case Nil => error("Expected to find net.liftweb.json.xschema2.XDefinition2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XField2ExtractorFunction: PartialFunction[JField, net.liftweb.json.xschema2.XField2] = List[PartialFunction[JField, net.liftweb.json.xschema2.XField2]](
      { case JField("XRealField2", value) => net.liftweb.json.xschema2.Serialization.XRealField2Extractor.extract(value) },
      { case JField("XViewField2", value) => net.liftweb.json.xschema2.Serialization.XViewField2Extractor.extract(value) },
      { case JField("XConstantField2", value) => net.liftweb.json.xschema2.Serialization.XConstantField2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XField2Extractor: Extractor[net.liftweb.json.xschema2.XField2] = new Extractor[net.liftweb.json.xschema2.XField2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XField2 = {
        (jvalue --> classOf[JObject]).obj.filter(XField2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XField2ExtractorFunction(field)
          case Nil => error("Expected to find net.liftweb.json.xschema2.XField2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XOrder2ExtractorFunction: PartialFunction[JField, net.liftweb.json.xschema2.XOrder2] = List[PartialFunction[JField, net.liftweb.json.xschema2.XOrder2]](
      { case JField("XOrderAscending2", value) => net.liftweb.json.xschema2.Serialization.XOrderAscending2Extractor.extract(value) },
      { case JField("XOrderDescending2", value) => net.liftweb.json.xschema2.Serialization.XOrderDescending2Extractor.extract(value) },
      { case JField("XOrderIgnore2", value) => net.liftweb.json.xschema2.Serialization.XOrderIgnore2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XOrder2Extractor: Extractor[net.liftweb.json.xschema2.XOrder2] = new Extractor[net.liftweb.json.xschema2.XOrder2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XOrder2 = {
        (jvalue --> classOf[JObject]).obj.filter(XOrder2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XOrder2ExtractorFunction(field)
          case Nil => error("Expected to find net.liftweb.json.xschema2.XOrder2 but found " + jvalue)
        }
      }
    }
    
    implicit val QualifiedName2Extractor: Extractor[net.liftweb.json.xschema2.QualifiedName2] = new Extractor[net.liftweb.json.xschema2.QualifiedName2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.QualifiedName2 = {
        QualifiedName2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JNothing)
        )
      }
    }
    
    implicit val XDefinitionRef2Extractor: Extractor[net.liftweb.json.xschema2.XDefinitionRef2] = new Extractor[net.liftweb.json.xschema2.XDefinitionRef2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XDefinitionRef2 = {
        XDefinitionRef2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JString(""))
        )
      }
    }
    
    implicit val XBoolean2Extractor: Extractor[net.liftweb.json.xschema2.XBoolean2.type] = new Extractor[net.liftweb.json.xschema2.XBoolean2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XBoolean2.type = {
        XBoolean2
      }
    }
    
    implicit val XInt2Extractor: Extractor[net.liftweb.json.xschema2.XInt2.type] = new Extractor[net.liftweb.json.xschema2.XInt2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XInt2.type = {
        XInt2
      }
    }
    
    implicit val XLong2Extractor: Extractor[net.liftweb.json.xschema2.XLong2.type] = new Extractor[net.liftweb.json.xschema2.XLong2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XLong2.type = {
        XLong2
      }
    }
    
    implicit val XFloat2Extractor: Extractor[net.liftweb.json.xschema2.XFloat2.type] = new Extractor[net.liftweb.json.xschema2.XFloat2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XFloat2.type = {
        XFloat2
      }
    }
    
    implicit val XDouble2Extractor: Extractor[net.liftweb.json.xschema2.XDouble2.type] = new Extractor[net.liftweb.json.xschema2.XDouble2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XDouble2.type = {
        XDouble2
      }
    }
    
    implicit val XString2Extractor: Extractor[net.liftweb.json.xschema2.XString2.type] = new Extractor[net.liftweb.json.xschema2.XString2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XString2.type = {
        XString2
      }
    }
    
    implicit val XJSON2Extractor: Extractor[net.liftweb.json.xschema2.XJSON2.type] = new Extractor[net.liftweb.json.xschema2.XJSON2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XJSON2.type = {
        XJSON2
      }
    }
    
    implicit val XList2Extractor: Extractor[net.liftweb.json.xschema2.XList2] = new Extractor[net.liftweb.json.xschema2.XList2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XList2 = {
        XList2(
          extractField[net.liftweb.json.xschema2.XReference2](jvalue, "elementType", JNothing)
        )
      }
    }
    
    implicit val XSet2Extractor: Extractor[net.liftweb.json.xschema2.XSet2] = new Extractor[net.liftweb.json.xschema2.XSet2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XSet2 = {
        XSet2(
          extractField[net.liftweb.json.xschema2.XReference2](jvalue, "elementType", JNothing)
        )
      }
    }
    
    implicit val XArray2Extractor: Extractor[net.liftweb.json.xschema2.XArray2] = new Extractor[net.liftweb.json.xschema2.XArray2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XArray2 = {
        XArray2(
          extractField[net.liftweb.json.xschema2.XReference2](jvalue, "elementType", JNothing)
        )
      }
    }
    
    implicit val XMap2Extractor: Extractor[net.liftweb.json.xschema2.XMap2] = new Extractor[net.liftweb.json.xschema2.XMap2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XMap2 = {
        XMap2(
          extractField[net.liftweb.json.xschema2.XReference2](jvalue, "keyType", JNothing),
          extractField[net.liftweb.json.xschema2.XReference2](jvalue, "valueType", JNothing)
        )
      }
    }
    
    implicit val XOptional2Extractor: Extractor[net.liftweb.json.xschema2.XOptional2] = new Extractor[net.liftweb.json.xschema2.XOptional2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XOptional2 = {
        XOptional2(
          extractField[net.liftweb.json.xschema2.XReference2](jvalue, "optionalType", JNothing)
        )
      }
    }
    
    implicit val XTuple2Extractor: Extractor[net.liftweb.json.xschema2.XTuple2] = new Extractor[net.liftweb.json.xschema2.XTuple2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XTuple2 = {
        XTuple2(
          extractField[List[net.liftweb.json.xschema2.XReference2]](jvalue, "types", JNothing)
        )
      }
    }
    
    implicit val XProduct2Extractor: Extractor[net.liftweb.json.xschema2.XProduct2] = new Extractor[net.liftweb.json.xschema2.XProduct2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XProduct2 = {
        XProduct2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JString("")),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[List[net.liftweb.json.xschema2.XReference2]](jvalue, "terms", JObject(Nil))
        )
      }
    }
    
    implicit val XCoproduct2Extractor: Extractor[net.liftweb.json.xschema2.XCoproduct2] = new Extractor[net.liftweb.json.xschema2.XCoproduct2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XCoproduct2 = {
        XCoproduct2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JString("")),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[List[net.liftweb.json.xschema2.XReference2]](jvalue, "terms", JObject(Nil))
        )
      }
    }
    
    implicit val XConstant2Extractor: Extractor[net.liftweb.json.xschema2.XConstant2] = new Extractor[net.liftweb.json.xschema2.XConstant2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XConstant2 = {
        XConstant2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JString("")),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[net.liftweb.json.JsonAST.JValue](jvalue, "default", JNothing)
        )
      }
    }
    
    implicit val XRealField2Extractor: Extractor[net.liftweb.json.xschema2.XRealField2] = new Extractor[net.liftweb.json.xschema2.XRealField2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XRealField2 = {
        XRealField2(
          extractField[String](jvalue, "name", JNothing),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[net.liftweb.json.JsonAST.JValue](jvalue, "default", JNothing),
          extractField[net.liftweb.json.xschema2.XOrder2](jvalue, "order", JObject(JField("XOrderAscending2",JObject(Nil))::Nil))
        )
      }
    }
    
    implicit val XViewField2Extractor: Extractor[net.liftweb.json.xschema2.XViewField2] = new Extractor[net.liftweb.json.xschema2.XViewField2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XViewField2 = {
        XViewField2(
          extractField[String](jvalue, "name", JNothing),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil))
        )
      }
    }
    
    implicit val XConstantField2Extractor: Extractor[net.liftweb.json.xschema2.XConstantField2] = new Extractor[net.liftweb.json.xschema2.XConstantField2] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XConstantField2 = {
        XConstantField2(
          extractField[String](jvalue, "name", JNothing),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[net.liftweb.json.JsonAST.JValue](jvalue, "default", JNothing)
        )
      }
    }
    
    implicit val XOrderAscending2Extractor: Extractor[net.liftweb.json.xschema2.XOrderAscending2.type] = new Extractor[net.liftweb.json.xschema2.XOrderAscending2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XOrderAscending2.type = {
        XOrderAscending2
      }
    }
    
    implicit val XOrderDescending2Extractor: Extractor[net.liftweb.json.xschema2.XOrderDescending2.type] = new Extractor[net.liftweb.json.xschema2.XOrderDescending2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XOrderDescending2.type = {
        XOrderDescending2
      }
    }
    
    implicit val XOrderIgnore2Extractor: Extractor[net.liftweb.json.xschema2.XOrderIgnore2.type] = new Extractor[net.liftweb.json.xschema2.XOrderIgnore2.type] {
      def extract(jvalue: JValue): net.liftweb.json.xschema2.XOrderIgnore2.type = {
        XOrderIgnore2
      }
    }
  }
  object Extractors extends Extractors
  
  trait Decomposers extends DefaultDecomposers with DecomposerHelpers {
    implicit val XSchema2Decomposer: Decomposer[net.liftweb.json.xschema2.XSchema2] = new Decomposer[net.liftweb.json.xschema2.XSchema2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XSchema2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XDefinition2 => JObject(JField("XDefinition2", net.liftweb.json.xschema2.Serialization.XDefinition2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XReference2 => JObject(JField("XReference2", net.liftweb.json.xschema2.Serialization.XReference2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XReference2Decomposer: Decomposer[net.liftweb.json.xschema2.XReference2] = new Decomposer[net.liftweb.json.xschema2.XReference2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XReference2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XPrimitiveRef2 => JObject(JField("XPrimitiveRef2", net.liftweb.json.xschema2.Serialization.XPrimitiveRef2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XDefinitionRef2 => JObject(JField("XDefinitionRef2", net.liftweb.json.xschema2.Serialization.XDefinitionRef2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XPrimitiveRef2Decomposer: Decomposer[net.liftweb.json.xschema2.XPrimitiveRef2] = new Decomposer[net.liftweb.json.xschema2.XPrimitiveRef2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XPrimitiveRef2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XBoolean2.type => JObject(JField("XBoolean2", net.liftweb.json.xschema2.Serialization.XBoolean2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XInt2.type => JObject(JField("XInt2", net.liftweb.json.xschema2.Serialization.XInt2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XLong2.type => JObject(JField("XLong2", net.liftweb.json.xschema2.Serialization.XLong2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XFloat2.type => JObject(JField("XFloat2", net.liftweb.json.xschema2.Serialization.XFloat2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XDouble2.type => JObject(JField("XDouble2", net.liftweb.json.xschema2.Serialization.XDouble2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XString2.type => JObject(JField("XString2", net.liftweb.json.xschema2.Serialization.XString2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XJSON2.type => JObject(JField("XJSON2", net.liftweb.json.xschema2.Serialization.XJSON2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XCollection2 => JObject(JField("XCollection2", net.liftweb.json.xschema2.Serialization.XCollection2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XMap2 => JObject(JField("XMap2", net.liftweb.json.xschema2.Serialization.XMap2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XOptional2 => JObject(JField("XOptional2", net.liftweb.json.xschema2.Serialization.XOptional2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XTuple2 => JObject(JField("XTuple2", net.liftweb.json.xschema2.Serialization.XTuple2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XCollection2Decomposer: Decomposer[net.liftweb.json.xschema2.XCollection2] = new Decomposer[net.liftweb.json.xschema2.XCollection2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XCollection2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XList2 => JObject(JField("XList2", net.liftweb.json.xschema2.Serialization.XList2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XSet2 => JObject(JField("XSet2", net.liftweb.json.xschema2.Serialization.XSet2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XArray2 => JObject(JField("XArray2", net.liftweb.json.xschema2.Serialization.XArray2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XDefinition2Decomposer: Decomposer[net.liftweb.json.xschema2.XDefinition2] = new Decomposer[net.liftweb.json.xschema2.XDefinition2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XDefinition2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XField2 => JObject(JField("XField2", net.liftweb.json.xschema2.Serialization.XField2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XProduct2 => JObject(JField("XProduct2", net.liftweb.json.xschema2.Serialization.XProduct2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XCoproduct2 => JObject(JField("XCoproduct2", net.liftweb.json.xschema2.Serialization.XCoproduct2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XConstant2 => JObject(JField("XConstant2", net.liftweb.json.xschema2.Serialization.XConstant2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XField2Decomposer: Decomposer[net.liftweb.json.xschema2.XField2] = new Decomposer[net.liftweb.json.xschema2.XField2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XField2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XRealField2 => JObject(JField("XRealField2", net.liftweb.json.xschema2.Serialization.XRealField2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XViewField2 => JObject(JField("XViewField2", net.liftweb.json.xschema2.Serialization.XViewField2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XConstantField2 => JObject(JField("XConstantField2", net.liftweb.json.xschema2.Serialization.XConstantField2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XOrder2Decomposer: Decomposer[net.liftweb.json.xschema2.XOrder2] = new Decomposer[net.liftweb.json.xschema2.XOrder2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XOrder2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XOrderAscending2.type => JObject(JField("XOrderAscending2", net.liftweb.json.xschema2.Serialization.XOrderAscending2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XOrderDescending2.type => JObject(JField("XOrderDescending2", net.liftweb.json.xschema2.Serialization.XOrderDescending2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XOrderIgnore2.type => JObject(JField("XOrderIgnore2", net.liftweb.json.xschema2.Serialization.XOrderIgnore2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val QualifiedName2Decomposer: Decomposer[net.liftweb.json.xschema2.QualifiedName2] = new Decomposer[net.liftweb.json.xschema2.QualifiedName2] {
      def decompose(tvalue: net.liftweb.json.xschema2.QualifiedName2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) :: Nil
        )
      }
    }
    
    implicit val XDefinitionRef2Decomposer: Decomposer[net.liftweb.json.xschema2.XDefinitionRef2] = new Decomposer[net.liftweb.json.xschema2.XDefinitionRef2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XDefinitionRef2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) :: Nil
        )
      }
    }
    
    implicit val XBoolean2Decomposer: Decomposer[net.liftweb.json.xschema2.XBoolean2.type] = new Decomposer[net.liftweb.json.xschema2.XBoolean2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XBoolean2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XInt2Decomposer: Decomposer[net.liftweb.json.xschema2.XInt2.type] = new Decomposer[net.liftweb.json.xschema2.XInt2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XInt2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XLong2Decomposer: Decomposer[net.liftweb.json.xschema2.XLong2.type] = new Decomposer[net.liftweb.json.xschema2.XLong2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XLong2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XFloat2Decomposer: Decomposer[net.liftweb.json.xschema2.XFloat2.type] = new Decomposer[net.liftweb.json.xschema2.XFloat2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XFloat2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XDouble2Decomposer: Decomposer[net.liftweb.json.xschema2.XDouble2.type] = new Decomposer[net.liftweb.json.xschema2.XDouble2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XDouble2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XString2Decomposer: Decomposer[net.liftweb.json.xschema2.XString2.type] = new Decomposer[net.liftweb.json.xschema2.XString2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XString2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XJSON2Decomposer: Decomposer[net.liftweb.json.xschema2.XJSON2.type] = new Decomposer[net.liftweb.json.xschema2.XJSON2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XJSON2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XList2Decomposer: Decomposer[net.liftweb.json.xschema2.XList2] = new Decomposer[net.liftweb.json.xschema2.XList2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XList2): JValue = {
        JObject(
          JField("elementType", tvalue.elementType.serialize) :: Nil
        )
      }
    }
    
    implicit val XSet2Decomposer: Decomposer[net.liftweb.json.xschema2.XSet2] = new Decomposer[net.liftweb.json.xschema2.XSet2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XSet2): JValue = {
        JObject(
          JField("elementType", tvalue.elementType.serialize) :: Nil
        )
      }
    }
    
    implicit val XArray2Decomposer: Decomposer[net.liftweb.json.xschema2.XArray2] = new Decomposer[net.liftweb.json.xschema2.XArray2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XArray2): JValue = {
        JObject(
          JField("elementType", tvalue.elementType.serialize) :: Nil
        )
      }
    }
    
    implicit val XMap2Decomposer: Decomposer[net.liftweb.json.xschema2.XMap2] = new Decomposer[net.liftweb.json.xschema2.XMap2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XMap2): JValue = {
        JObject(
          JField("keyType", tvalue.keyType.serialize) ::
          JField("valueType", tvalue.valueType.serialize) :: Nil
        )
      }
    }
    
    implicit val XOptional2Decomposer: Decomposer[net.liftweb.json.xschema2.XOptional2] = new Decomposer[net.liftweb.json.xschema2.XOptional2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XOptional2): JValue = {
        JObject(
          JField("optionalType", tvalue.optionalType.serialize) :: Nil
        )
      }
    }
    
    implicit val XTuple2Decomposer: Decomposer[net.liftweb.json.xschema2.XTuple2] = new Decomposer[net.liftweb.json.xschema2.XTuple2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XTuple2): JValue = {
        JObject(
          JField("types", tvalue.types.serialize) :: Nil
        )
      }
    }
    
    implicit val XProduct2Decomposer: Decomposer[net.liftweb.json.xschema2.XProduct2] = new Decomposer[net.liftweb.json.xschema2.XProduct2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XProduct2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("terms", tvalue.terms.serialize) :: Nil
        )
      }
    }
    
    implicit val XCoproduct2Decomposer: Decomposer[net.liftweb.json.xschema2.XCoproduct2] = new Decomposer[net.liftweb.json.xschema2.XCoproduct2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XCoproduct2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("terms", tvalue.terms.serialize) :: Nil
        )
      }
    }
    
    implicit val XConstant2Decomposer: Decomposer[net.liftweb.json.xschema2.XConstant2] = new Decomposer[net.liftweb.json.xschema2.XConstant2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XConstant2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("default", tvalue.default.serialize) :: Nil
        )
      }
    }
    
    implicit val XRealField2Decomposer: Decomposer[net.liftweb.json.xschema2.XRealField2] = new Decomposer[net.liftweb.json.xschema2.XRealField2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XRealField2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("default", tvalue.default.serialize) ::
          JField("order", tvalue.order.serialize) :: Nil
        )
      }
    }
    
    implicit val XViewField2Decomposer: Decomposer[net.liftweb.json.xschema2.XViewField2] = new Decomposer[net.liftweb.json.xschema2.XViewField2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XViewField2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("properties", tvalue.properties.serialize) :: Nil
        )
      }
    }
    
    implicit val XConstantField2Decomposer: Decomposer[net.liftweb.json.xschema2.XConstantField2] = new Decomposer[net.liftweb.json.xschema2.XConstantField2] {
      def decompose(tvalue: net.liftweb.json.xschema2.XConstantField2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("default", tvalue.default.serialize) :: Nil
        )
      }
    }
    
    implicit val XOrderAscending2Decomposer: Decomposer[net.liftweb.json.xschema2.XOrderAscending2.type] = new Decomposer[net.liftweb.json.xschema2.XOrderAscending2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XOrderAscending2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XOrderDescending2Decomposer: Decomposer[net.liftweb.json.xschema2.XOrderDescending2.type] = new Decomposer[net.liftweb.json.xschema2.XOrderDescending2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XOrderDescending2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XOrderIgnore2Decomposer: Decomposer[net.liftweb.json.xschema2.XOrderIgnore2.type] = new Decomposer[net.liftweb.json.xschema2.XOrderIgnore2.type] {
      def decompose(tvalue: net.liftweb.json.xschema2.XOrderIgnore2.type): JValue = {
        JObject(
           Nil
        )
      }
    }
  }
  object Decomposers extends Decomposers
  
  object Serialization extends SerializationImplicits with Decomposers with Extractors with Orderings {
    import XSchemaAST.{XRoot, XSchema}
  }
  
  object Constants {
    import Serialization._
    
    
  }
}