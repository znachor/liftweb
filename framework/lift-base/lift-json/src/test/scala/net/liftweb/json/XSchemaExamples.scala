/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package json {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class XSchemaExamplesTest extends Runner(XSchemaExamples) with JUnit
object XSchemaExamples extends Specification {
  import JsonAST._
  import XSchema._
  
  "Primitives can be extracted from strings" in {
    intExtractor(JString("12")) mustEqual 12
    longExtractor(JString("12")) mustEqual 12    
    floatExtractor(JString("12.5")) mustEqual 12.5F
    doubleExtractor(JString("12.5")) mustEqual 12.5
    booleanExtractor(JString("true")) mustEqual true
    booleanExtractor(JString("false")) mustEqual false
    booleanExtractor(JString("0")) mustEqual false
    booleanExtractor(JString("1")) mustEqual true
  }
  
  "Reals can be extracted from integers" in {
    floatExtractor(JInt(12)) mustEqual 12.0F
    doubleExtractor(JInt(12)) mustEqual 12.0
  }
  
  "Booleans can be extracted from integers" in {
    booleanExtractor(JInt(0)) mustEqual false
    booleanExtractor(JInt(1)) mustEqual true
  }
  
  "Integers can be extracted from reals" in {
    intExtractor(JDouble(12.0)) mustEqual 12
    longExtractor(JDouble(12.0)) mustEqual 12L
  }
  
  def testSymmetry[T](t: T)(implicit d: Decomposer[T], e: Extractor[T]) = {
    t.serialize.deserialize[T] mustEqual t
  }
  
  "Primitive serialization is symmetric" in {
    testSymmetry(true)
    testSymmetry("foo")
    testSymmetry(1)
    testSymmetry(1L)
    testSymmetry(1.5F)
    testSymmetry(1.5)
    testSymmetry(List("foo"))
    testSymmetry(Set("foo"))
    testSymmetry[Option[String]](None)
    testSymmetry[Option[String]](Some("foo"))
    testSymmetry(("foo", 12))
    testSymmetry(("foo", 12, 3.2F))
    testSymmetry(("foo", 12, 3.2F, "blah"))
    testSymmetry(("foo", 12, 3.2F, "blah", "last-one"))
    testSymmetry(Map("foo" -> 123, "bar" -> 329))
    
    var array = Array("foo")
    array.serialize.deserialize[Array[String]].toList mustEqual array.toList
  }
  
  "Custom product serialization is symmetric" in {
    import data.social._
    import data.social.Serialization._
    
    testSymmetry(Male("foobar"))
    testSymmetry(Female("baz"))
  }
  
  "Custom defaults are valid" in {
    import data.social.Constants._
    
    DefaultFemale mustEqual DefaultFemale
    DefaultMale mustEqual DefaultMale
  }
}


package data.social {
  
  import net.liftweb.json.{SerializationImplicits, DefaultExtractors, ExtractionHelpers, DefaultDecomposers, DecomposerHelpers, DefaultOrderings}
  import net.liftweb.json.JsonParser._
  import net.liftweb.json.JsonAST._
  import net.liftweb.json.XSchema._
  
  /** This is the coproduct that includes male and female. The normal way to
   * translate this into OOP is as a superclass/superinterface.
   */
  sealed trait Gender extends Product with java.io.Serializable with java.lang.Cloneable {
    def text: String
  }
  
  case class Male(text: String) extends Ordered[Male] with data.social.Gender with java.io.Serializable with java.lang.Cloneable {
    def compare(that: Male): Int = {
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.text.compare(that.text)
      if (c != 0) return c * -1
      
      return 0
    }
  }
  
  case class Female(text: String) extends Ordered[Female] with data.social.Gender with java.io.Serializable with java.lang.Cloneable {
    def compare(that: Female): Int = {
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.text.compare(that.text)
      if (c != 0) return c * 1
      
      return 0
    }
  }
  
  trait Extractors extends DefaultExtractors with ExtractionHelpers {
    private lazy val GenderExtractorFunction: PartialFunction[JField, Gender] = List[PartialFunction[JField, Gender]](
      { case JField("Male", value) => data.social.Serialization.MaleExtractor.extract(value) },
      { case JField("Female", value) => data.social.Serialization.FemaleExtractor.extract(value) }
    ).reduceLeft { (a, b) => a.orElse(b) }
    
    implicit val GenderExtractor: Extractor[Gender] = new Extractor[Gender] {
      def extract(jvalue: JValue): Gender = {
        (jvalue --> classOf[JObject]).obj.filter(GenderExtractorFunction.isDefinedAt _) match {
          case field :: fields => GenderExtractorFunction(field)
          case Nil => error("Expected to find Gender but found " + jvalue)
        }
      }
    }
    
    implicit val MaleExtractor: Extractor[Male] = new Extractor[Male] {
      def extract(jvalue: JValue): Male = {
        Male(
          extractField[String](jvalue, "text", """"male" """)
        )
      }
    }
    
    implicit val FemaleExtractor: Extractor[Female] = new Extractor[Female] {
      def extract(jvalue: JValue): Female = {
        Female(
          extractField[String](jvalue, "text", """"female" """)
        )
      }
    }
  }
  
  trait Decomposers extends DefaultDecomposers with DecomposerHelpers {
    implicit val GenderDecomposer: Decomposer[Gender] = new Decomposer[Gender] {
      def decompose(tvalue: Gender): JValue = {
        tvalue match {
          case x: data.social.Male => JObject(JField("Male", data.social.Serialization.MaleDecomposer.decompose(x)) :: Nil)
          case x: data.social.Female => JObject(JField("Female", data.social.Serialization.FemaleDecomposer.decompose(x)) :: Nil)
          
        }
      }
    }
    
    implicit val MaleDecomposer: Decomposer[Male] = new Decomposer[Male] {
      def decompose(tvalue: Male): JValue = {
        JObject(
          JField("text", tvalue.text.serialize) ::
          Nil
        )
      }
    }
    
    implicit val FemaleDecomposer: Decomposer[Female] = new Decomposer[Female] {
      def decompose(tvalue: Female): JValue = {
        JObject(
          JField("text", tvalue.text.serialize) ::
          Nil
        )
      }
    }
  }
  
  object Serialization extends SerializationImplicits with Decomposers with Extractors { }
  
  object Constants {
    import Serialization._
    
    lazy val DefaultFemale = parse("""{"Female":{"text":"female"}} """).deserialize[data.social.Gender]
    lazy val DefaultMale = parse("""{"Male":{"text":"male"}} """).deserialize[data.social.Gender]
  }
}


}
}
