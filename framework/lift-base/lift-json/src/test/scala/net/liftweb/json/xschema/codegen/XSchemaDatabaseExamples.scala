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
  
  ScalaCodeGenerator.generate(XSchemaSchema, ".")
  
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





package data.social {
  import net.liftweb.json.JsonParser._
  import net.liftweb.json.JsonAST._
  import net.liftweb.json.xschema.{XRoot, XProduct, XCoproduct, XSchemaDerived, SerializationImplicits, Extractor, ExtractionHelpers, Decomposer, DecomposerHelpers, DefaultExtractors, DefaultDecomposers, DefaultOrderings}
  import net.liftweb.json.xschema.DefaultSerialization._
  import net.liftweb.json.xschema.{SerializationImplicits => XSerializationImplicits, DefaultExtractors => XDefaultExtractors}
  import java.lang.reflect._
  
  
  
  trait Orderings {
    implicit def GenderToOrderedGender(inner: Gender) = OrderedGender(inner)
    implicit def TimeToOrderedTime(inner: Time) = OrderedTime(inner)
    
    case class OrderedGender(inner: data.social.Gender) extends Ordered[data.social.Gender] {
      def compare(that: data.social.Gender): Int = {
        if (inner == that) 0
        else inner match {
          case x: data.social.Male => that match {
            case y: data.social.Male => x.compare(y)
            case y: data.social.Female => -1
          }
          case x: data.social.Female => that match {
            case y: data.social.Male => 1
            case y: data.social.Female => x.compare(y)
          }
        }
      }
    }
    
    case class OrderedTime(inner: data.social.Time) extends Ordered[data.social.Time] {
      def compare(that: data.social.Time): Int = {
        if (inner == that) 0
        else inner match {
          case x: data.social.Morning.type => that match {
            case y: data.social.Morning.type => x.compare(y)
            case y: data.social.Noon.type => -1
            case y: data.social.Night.type => -1
          }
          case x: data.social.Noon.type => that match {
            case y: data.social.Morning.type => 1
            case y: data.social.Noon.type => x.compare(y)
            case y: data.social.Night.type => -1
          }
          case x: data.social.Night.type => that match {
            case y: data.social.Morning.type => 1
            case y: data.social.Noon.type => 1
            case y: data.social.Night.type => x.compare(y)
          }
        }
      }
    }
  }
  object Orderings extends Orderings
  
  /** This is the coproduct that includes male and female. The normal way to
   * translate this into OOP is as a superclass/superinterface.
   */
  sealed trait Gender extends Product with java.io.Serializable with java.lang.Cloneable {
    def text: String
  }
  object Gender extends java.io.Serializable with java.lang.Cloneable {
    
  }
  
  sealed trait Time extends Product {
    
  }
  
  case class Male(text: String) extends Ordered[data.social.Male] with data.social.Gender with java.io.Serializable with java.lang.Cloneable {
    def compare(that: data.social.Male): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.text.compare(that.text)
      if (c != 0) return c * -1
      
      return this.hashCode - that.hashCode
    }
    def asFemale: data.social.Female = data.social.Female(text)
  }
  
  case class Female(text: String) extends Ordered[data.social.Female] with data.social.Gender with java.io.Serializable with java.lang.Cloneable {
    def compare(that: data.social.Female): Int = {
      import Orderings._
      
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.text.compare(that.text)
      if (c != 0) return c * 1
      
      return this.hashCode - that.hashCode
    }
    def asMale: data.social.Male = data.social.Male(text)
  }
  
  case object Morning extends data.social.Time 
  
  case object Noon extends data.social.Time 
  
  case object Night extends data.social.Time 
  
  trait Extractors extends DefaultExtractors with ExtractionHelpers {
    private lazy val GenderExtractorFunction: PartialFunction[JField, data.social.Gender] = List[PartialFunction[JField, data.social.Gender]](
      { case JField("Male", value) => data.social.Serialization.MaleExtractor.extract(value) },
      { case JField("Female", value) => data.social.Serialization.FemaleExtractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val GenderExtractor: Extractor[data.social.Gender] = new Extractor[data.social.Gender] {
      def extract(jvalue: JValue): data.social.Gender = {
        (jvalue --> classOf[JObject]).obj.filter(GenderExtractorFunction.isDefinedAt _) match {
          case field :: fields => GenderExtractorFunction(field)
          case Nil => error("Expected to find data.social.Gender but found " + jvalue)
        }
      }
    }
    
    private lazy val TimeExtractorFunction: PartialFunction[JField, data.social.Time] = List[PartialFunction[JField, data.social.Time]](
      { case JField("Morning", value) => data.social.Serialization.MorningExtractor.extract(value) },
      { case JField("Noon", value) => data.social.Serialization.NoonExtractor.extract(value) },
      { case JField("Night", value) => data.social.Serialization.NightExtractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val TimeExtractor: Extractor[data.social.Time] = new Extractor[data.social.Time] {
      def extract(jvalue: JValue): data.social.Time = {
        (jvalue --> classOf[JObject]).obj.filter(TimeExtractorFunction.isDefinedAt _) match {
          case field :: fields => TimeExtractorFunction(field)
          case Nil => error("Expected to find data.social.Time but found " + jvalue)
        }
      }
    }
    
    implicit val MaleExtractor: Extractor[data.social.Male] = new Extractor[data.social.Male] {
      def extract(jvalue: JValue): data.social.Male = {
        Male(
          extractField[String](jvalue, "text", JString("male"))
        )
      }
    }
    
    implicit val FemaleExtractor: Extractor[data.social.Female] = new Extractor[data.social.Female] {
      def extract(jvalue: JValue): data.social.Female = {
        Female(
          extractField[String](jvalue, "text", JString("female"))
        )
      }
    }
    
    implicit val MorningExtractor: Extractor[data.social.Morning.type] = new Extractor[data.social.Morning.type] {
      def extract(jvalue: JValue): data.social.Morning.type = {
        Morning
      }
    }
    
    implicit val NoonExtractor: Extractor[data.social.Noon.type] = new Extractor[data.social.Noon.type] {
      def extract(jvalue: JValue): data.social.Noon.type = {
        Noon
      }
    }
    
    implicit val NightExtractor: Extractor[data.social.Night.type] = new Extractor[data.social.Night.type] {
      def extract(jvalue: JValue): data.social.Night.type = {
        Night
      }
    }
  }
  object Extractors extends Extractors
  
  trait Decomposers extends DefaultDecomposers with DecomposerHelpers {
    implicit val GenderDecomposer: Decomposer[data.social.Gender] = new Decomposer[data.social.Gender] {
      def decompose(tvalue: data.social.Gender): JValue = {
        tvalue match {
          case x: data.social.Male => JObject(JField("Male", data.social.Serialization.MaleDecomposer.decompose(x)) :: Nil)
          case x: data.social.Female => JObject(JField("Female", data.social.Serialization.FemaleDecomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val TimeDecomposer: Decomposer[data.social.Time] = new Decomposer[data.social.Time] {
      def decompose(tvalue: data.social.Time): JValue = {
        tvalue match {
          case x: data.social.Morning.type => JObject(JField("Morning", data.social.Serialization.MorningDecomposer.decompose(x)) :: Nil)
          case x: data.social.Noon.type => JObject(JField("Noon", data.social.Serialization.NoonDecomposer.decompose(x)) :: Nil)
          case x: data.social.Night.type => JObject(JField("Night", data.social.Serialization.NightDecomposer.decompose(x)) :: Nil)
        }
      }
    }
    
    implicit val MaleDecomposer: Decomposer[data.social.Male] = new Decomposer[data.social.Male] {
      def decompose(tvalue: data.social.Male): JValue = {
        JObject(
          JField("text", tvalue.text.serialize) :: Nil
        )
      }
    }
    
    implicit val FemaleDecomposer: Decomposer[data.social.Female] = new Decomposer[data.social.Female] {
      def decompose(tvalue: data.social.Female): JValue = {
        JObject(
          JField("text", tvalue.text.serialize) :: Nil
        )
      }
    }
    
    implicit val MorningDecomposer: Decomposer[data.social.Morning.type] = new Decomposer[data.social.Morning.type] {
      def decompose(tvalue: data.social.Morning.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val NoonDecomposer: Decomposer[data.social.Noon.type] = new Decomposer[data.social.Noon.type] {
      def decompose(tvalue: data.social.Noon.type): JValue = {
        JObject(
           Nil
        )
      }
    }
    
    implicit val NightDecomposer: Decomposer[data.social.Night.type] = new Decomposer[data.social.Night.type] {
      def decompose(tvalue: data.social.Night.type): JValue = {
        JObject(
           Nil
        )
      }
    }
  }
  object Decomposers extends Decomposers
  
  object Serialization extends SerializationImplicits with Decomposers with Extractors with Orderings {
    
  }
  
  object Constants {
    import Serialization._
    
    lazy val DefaultFemale = JObject(JField("Female",JObject(JField("text",JString("female"))::Nil))::Nil).deserialize[data.social.Gender]
    lazy val DefaultMale = JObject(JField("Male",JObject(JField("text",JString("male"))::Nil))::Nil).deserialize[data.social.Gender]
  }
}