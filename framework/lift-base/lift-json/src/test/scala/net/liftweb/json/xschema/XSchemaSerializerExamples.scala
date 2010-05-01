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
    implicit def XDefinition2ToOrderedXDefinition2(inner: XDefinition2) = OrderedXDefinition2(inner)
    implicit def XField2ToOrderedXField2(inner: XField2) = OrderedXField2(inner)
    implicit def XOrder2ToOrderedXOrder2(inner: XOrder2) = OrderedXOrder2(inner)
    
    case class OrderedXSchema2(inner: XSchema2) extends Ordered[XSchema2] {
      def compare(that: XSchema2): Int = {
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
    
    case class OrderedXDefinition2(inner: XDefinition2) extends Ordered[XDefinition2] {
      def compare(that: XDefinition2): Int = {
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
    
    case class OrderedXField2(inner: XField2) extends Ordered[XField2] {
      def compare(that: XField2): Int = {
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
    
    case class OrderedXOrder2(inner: XOrder2) extends Ordered[XOrder2] {
      def compare(that: XOrder2): Int = {
        if (inner == that) 0
        else inner match {
          case x: net.liftweb.json.xschema2.XOrderAscending2 => that match {
            case y: net.liftweb.json.xschema2.XOrderAscending2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XOrderDescending2 => -1
            case y: net.liftweb.json.xschema2.XOrderIgnore2 => -1
          }
          case x: net.liftweb.json.xschema2.XOrderDescending2 => that match {
            case y: net.liftweb.json.xschema2.XOrderAscending2 => 1
            case y: net.liftweb.json.xschema2.XOrderDescending2 => x.compare(y)
            case y: net.liftweb.json.xschema2.XOrderIgnore2 => -1
          }
          case x: net.liftweb.json.xschema2.XOrderIgnore2 => that match {
            case y: net.liftweb.json.xschema2.XOrderAscending2 => 1
            case y: net.liftweb.json.xschema2.XOrderDescending2 => 1
            case y: net.liftweb.json.xschema2.XOrderIgnore2 => x.compare(y)
          }
        }
      }
    }
  }
  object Orderings extends Orderings
  
  sealed trait XSchema2 extends Product {
    def name: String
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
  
  case class QualifiedName2(name: String, namespace: String) extends Ordered[QualifiedName2] {
    def compare(that: QualifiedName2): Int = {
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
  
  case class XReference2(name: String, namespace: String) extends Ordered[XReference2] with net.liftweb.json.xschema2.XSchema2 {
    def compare(that: XReference2): Int = {
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
  
  case class XProduct2(name: String, namespace: String, properties: Map[String, String], terms: List[net.liftweb.json.xschema2.XReference2]) extends Ordered[XProduct2] with net.liftweb.json.xschema2.XDefinition2 {
    def compare(that: XProduct2): Int = {
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
  
  case class XCoproduct2(name: String, namespace: String, properties: Map[String, String], terms: List[net.liftweb.json.xschema2.XReference2]) extends Ordered[XCoproduct2] with net.liftweb.json.xschema2.XDefinition2 {
    def compare(that: XCoproduct2): Int = {
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
  
  case class XConstant2(name: String, namespace: String, properties: Map[String, String], default: net.liftweb.json.JsonAST.JValue) extends Ordered[XConstant2] with net.liftweb.json.xschema2.XDefinition2 {
    def compare(that: XConstant2): Int = {
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
  
  case class XRealField2(name: String, properties: Map[String, String], default: net.liftweb.json.JsonAST.JValue, order: net.liftweb.json.xschema2.XOrder2) extends Ordered[XRealField2] with net.liftweb.json.xschema2.XField2 {
    def compare(that: XRealField2): Int = {
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
  
  case class XViewField2(name: String, properties: Map[String, String]) extends Ordered[XViewField2] with net.liftweb.json.xschema2.XField2 {
    def compare(that: XViewField2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.name.compare(that.name)
      if (c != 0) return c * 1
      
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XConstantField2(name: String, properties: Map[String, String], default: net.liftweb.json.JsonAST.JValue) extends Ordered[XConstantField2] with net.liftweb.json.xschema2.XField2 {
    def compare(that: XConstantField2): Int = {
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
  
  case class XOrderAscending2() extends Ordered[XOrderAscending2] with net.liftweb.json.xschema2.XOrder2 {
    def compare(that: XOrderAscending2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XOrderDescending2() extends Ordered[XOrderDescending2] with net.liftweb.json.xschema2.XOrder2 {
    def compare(that: XOrderDescending2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  case class XOrderIgnore2() extends Ordered[XOrderIgnore2] with net.liftweb.json.xschema2.XOrder2 {
    def compare(that: XOrderIgnore2): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      
      return this.hashCode - that.hashCode
    }
    
  }
  
  trait Extractors extends DefaultExtractors with ExtractionHelpers {
    private lazy val XSchema2ExtractorFunction: PartialFunction[JField, XSchema2] = List[PartialFunction[JField, XSchema2]](
      { case JField("XDefinition2", value) => net.liftweb.json.xschema2.Serialization.XDefinition2Extractor.extract(value) },
      { case JField("XReference2", value) => net.liftweb.json.xschema2.Serialization.XReference2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XSchema2Extractor: Extractor[XSchema2] = new Extractor[XSchema2] {
      def extract(jvalue: JValue): XSchema2 = {
        (jvalue --> classOf[JObject]).obj.filter(XSchema2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XSchema2ExtractorFunction(field)
          case Nil => error("Expected to find XSchema2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XDefinition2ExtractorFunction: PartialFunction[JField, XDefinition2] = List[PartialFunction[JField, XDefinition2]](
      { case JField("XField2", value) => net.liftweb.json.xschema2.Serialization.XField2Extractor.extract(value) },
      { case JField("XProduct2", value) => net.liftweb.json.xschema2.Serialization.XProduct2Extractor.extract(value) },
      { case JField("XCoproduct2", value) => net.liftweb.json.xschema2.Serialization.XCoproduct2Extractor.extract(value) },
      { case JField("XConstant2", value) => net.liftweb.json.xschema2.Serialization.XConstant2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XDefinition2Extractor: Extractor[XDefinition2] = new Extractor[XDefinition2] {
      def extract(jvalue: JValue): XDefinition2 = {
        (jvalue --> classOf[JObject]).obj.filter(XDefinition2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XDefinition2ExtractorFunction(field)
          case Nil => error("Expected to find XDefinition2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XField2ExtractorFunction: PartialFunction[JField, XField2] = List[PartialFunction[JField, XField2]](
      { case JField("XRealField2", value) => net.liftweb.json.xschema2.Serialization.XRealField2Extractor.extract(value) },
      { case JField("XViewField2", value) => net.liftweb.json.xschema2.Serialization.XViewField2Extractor.extract(value) },
      { case JField("XConstantField2", value) => net.liftweb.json.xschema2.Serialization.XConstantField2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XField2Extractor: Extractor[XField2] = new Extractor[XField2] {
      def extract(jvalue: JValue): XField2 = {
        (jvalue --> classOf[JObject]).obj.filter(XField2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XField2ExtractorFunction(field)
          case Nil => error("Expected to find XField2 but found " + jvalue)
        }
      }
    }
    
    private lazy val XOrder2ExtractorFunction: PartialFunction[JField, XOrder2] = List[PartialFunction[JField, XOrder2]](
      { case JField("XOrderAscending2", value) => net.liftweb.json.xschema2.Serialization.XOrderAscending2Extractor.extract(value) },
      { case JField("XOrderDescending2", value) => net.liftweb.json.xschema2.Serialization.XOrderDescending2Extractor.extract(value) },
      { case JField("XOrderIgnore2", value) => net.liftweb.json.xschema2.Serialization.XOrderIgnore2Extractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val XOrder2Extractor: Extractor[XOrder2] = new Extractor[XOrder2] {
      def extract(jvalue: JValue): XOrder2 = {
        (jvalue --> classOf[JObject]).obj.filter(XOrder2ExtractorFunction.isDefinedAt _) match {
          case field :: fields => XOrder2ExtractorFunction(field)
          case Nil => error("Expected to find XOrder2 but found " + jvalue)
        }
      }
    }
    
    implicit val QualifiedName2Extractor: Extractor[QualifiedName2] = new Extractor[QualifiedName2] {
      def extract(jvalue: JValue): QualifiedName2 = {
        QualifiedName2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JNothing)
        )
      }
    }
    
    implicit val XReference2Extractor: Extractor[XReference2] = new Extractor[XReference2] {
      def extract(jvalue: JValue): XReference2 = {
        XReference2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JString(""))
        )
      }
    }
    
    implicit val XProduct2Extractor: Extractor[XProduct2] = new Extractor[XProduct2] {
      def extract(jvalue: JValue): XProduct2 = {
        XProduct2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JString("")),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[List[net.liftweb.json.xschema2.XReference2]](jvalue, "terms", JObject(Nil))
        )
      }
    }
    
    implicit val XCoproduct2Extractor: Extractor[XCoproduct2] = new Extractor[XCoproduct2] {
      def extract(jvalue: JValue): XCoproduct2 = {
        XCoproduct2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JString("")),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[List[net.liftweb.json.xschema2.XReference2]](jvalue, "terms", JObject(Nil))
        )
      }
    }
    
    implicit val XConstant2Extractor: Extractor[XConstant2] = new Extractor[XConstant2] {
      def extract(jvalue: JValue): XConstant2 = {
        XConstant2(
          extractField[String](jvalue, "name", JNothing),
          extractField[String](jvalue, "namespace", JString("")),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[net.liftweb.json.JsonAST.JValue](jvalue, "default", JNothing)
        )
      }
    }
    
    implicit val XRealField2Extractor: Extractor[XRealField2] = new Extractor[XRealField2] {
      def extract(jvalue: JValue): XRealField2 = {
        XRealField2(
          extractField[String](jvalue, "name", JNothing),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[net.liftweb.json.JsonAST.JValue](jvalue, "default", JNothing),
          extractField[net.liftweb.json.xschema2.XOrder2](jvalue, "order", JObject(JField("XOrderAscending2",JObject(Nil))::Nil))
        )
      }
    }
    
    implicit val XViewField2Extractor: Extractor[XViewField2] = new Extractor[XViewField2] {
      def extract(jvalue: JValue): XViewField2 = {
        XViewField2(
          extractField[String](jvalue, "name", JNothing),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil))
        )
      }
    }
    
    implicit val XConstantField2Extractor: Extractor[XConstantField2] = new Extractor[XConstantField2] {
      def extract(jvalue: JValue): XConstantField2 = {
        XConstantField2(
          extractField[String](jvalue, "name", JNothing),
          extractField[Map[String, String]](jvalue, "properties", JArray(Nil)),
          extractField[net.liftweb.json.JsonAST.JValue](jvalue, "default", JNothing)
        )
      }
    }
    
    implicit val XOrderAscending2Extractor: Extractor[XOrderAscending2] = new Extractor[XOrderAscending2] {
      def extract(jvalue: JValue): XOrderAscending2 = {
        XOrderAscending2(
          
        )
      }
    }
    
    implicit val XOrderDescending2Extractor: Extractor[XOrderDescending2] = new Extractor[XOrderDescending2] {
      def extract(jvalue: JValue): XOrderDescending2 = {
        XOrderDescending2(
          
        )
      }
    }
    
    implicit val XOrderIgnore2Extractor: Extractor[XOrderIgnore2] = new Extractor[XOrderIgnore2] {
      def extract(jvalue: JValue): XOrderIgnore2 = {
        XOrderIgnore2(
          
        )
      }
    }
  }
  object Extractors extends Extractors
  
  trait Decomposers extends DefaultDecomposers with DecomposerHelpers {
    implicit val XSchema2Decomposer: Decomposer[XSchema2] = new Decomposer[XSchema2] {
      def decompose(tvalue: XSchema2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XDefinition2 => JObject(JField("XDefinition2", net.liftweb.json.xschema2.Serialization.XDefinition2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XReference2 => JObject(JField("XReference2", net.liftweb.json.xschema2.Serialization.XReference2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XDefinition2Decomposer: Decomposer[XDefinition2] = new Decomposer[XDefinition2] {
      def decompose(tvalue: XDefinition2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XField2 => JObject(JField("XField2", net.liftweb.json.xschema2.Serialization.XField2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XProduct2 => JObject(JField("XProduct2", net.liftweb.json.xschema2.Serialization.XProduct2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XCoproduct2 => JObject(JField("XCoproduct2", net.liftweb.json.xschema2.Serialization.XCoproduct2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XConstant2 => JObject(JField("XConstant2", net.liftweb.json.xschema2.Serialization.XConstant2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XField2Decomposer: Decomposer[XField2] = new Decomposer[XField2] {
      def decompose(tvalue: XField2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XRealField2 => JObject(JField("XRealField2", net.liftweb.json.xschema2.Serialization.XRealField2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XViewField2 => JObject(JField("XViewField2", net.liftweb.json.xschema2.Serialization.XViewField2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XConstantField2 => JObject(JField("XConstantField2", net.liftweb.json.xschema2.Serialization.XConstantField2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val XOrder2Decomposer: Decomposer[XOrder2] = new Decomposer[XOrder2] {
      def decompose(tvalue: XOrder2): JValue = {
        tvalue match {
          case x: net.liftweb.json.xschema2.XOrderAscending2 => JObject(JField("XOrderAscending2", net.liftweb.json.xschema2.Serialization.XOrderAscending2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XOrderDescending2 => JObject(JField("XOrderDescending2", net.liftweb.json.xschema2.Serialization.XOrderDescending2Decomposer.decompose(x)) :: Nil)
          case x: net.liftweb.json.xschema2.XOrderIgnore2 => JObject(JField("XOrderIgnore2", net.liftweb.json.xschema2.Serialization.XOrderIgnore2Decomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val QualifiedName2Decomposer: Decomposer[QualifiedName2] = new Decomposer[QualifiedName2] {
      def decompose(tvalue: QualifiedName2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) :: Nil
        )
      }
    }
    
    implicit val XReference2Decomposer: Decomposer[XReference2] = new Decomposer[XReference2] {
      def decompose(tvalue: XReference2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) :: Nil
        )
      }
    }
    
    implicit val XProduct2Decomposer: Decomposer[XProduct2] = new Decomposer[XProduct2] {
      def decompose(tvalue: XProduct2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("terms", tvalue.terms.serialize) :: Nil
        )
      }
    }
    
    implicit val XCoproduct2Decomposer: Decomposer[XCoproduct2] = new Decomposer[XCoproduct2] {
      def decompose(tvalue: XCoproduct2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("terms", tvalue.terms.serialize) :: Nil
        )
      }
    }
    
    implicit val XConstant2Decomposer: Decomposer[XConstant2] = new Decomposer[XConstant2] {
      def decompose(tvalue: XConstant2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("namespace", tvalue.namespace.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("default", tvalue.default.serialize) :: Nil
        )
      }
    }
    
    implicit val XRealField2Decomposer: Decomposer[XRealField2] = new Decomposer[XRealField2] {
      def decompose(tvalue: XRealField2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("default", tvalue.default.serialize) ::
          JField("order", tvalue.order.serialize) :: Nil
        )
      }
    }
    
    implicit val XViewField2Decomposer: Decomposer[XViewField2] = new Decomposer[XViewField2] {
      def decompose(tvalue: XViewField2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("properties", tvalue.properties.serialize) :: Nil
        )
      }
    }
    
    implicit val XConstantField2Decomposer: Decomposer[XConstantField2] = new Decomposer[XConstantField2] {
      def decompose(tvalue: XConstantField2): JValue = {
        JObject(
          JField("name", tvalue.name.serialize) ::
          JField("properties", tvalue.properties.serialize) ::
          JField("default", tvalue.default.serialize) :: Nil
        )
      }
    }
    
    implicit val XOrderAscending2Decomposer: Decomposer[XOrderAscending2] = new Decomposer[XOrderAscending2] {
      def decompose(tvalue: XOrderAscending2): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XOrderDescending2Decomposer: Decomposer[XOrderDescending2] = new Decomposer[XOrderDescending2] {
      def decompose(tvalue: XOrderDescending2): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val XOrderIgnore2Decomposer: Decomposer[XOrderIgnore2] = new Decomposer[XOrderIgnore2] {
      def decompose(tvalue: XOrderIgnore2): JValue = {
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