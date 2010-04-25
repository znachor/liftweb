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
    import DemoData._
    import DemoData.Serialization._
    
    testSymmetry(DemoProduct("baz", 123))
  }
}

private[json] object DemoData {
  import JsonAST._
  import JsonParser._
  import XSchema._
  
  case class DemoProduct(foo: String, bar: Int) extends Ordered[DemoProduct] {
    def compare(that: DemoProduct): Int = {
      if (this == that) return 0
      
      var c: Int = 0
      
      c = this.foo.compare(that.foo)      
      if (c != 0) return c
      
      c = this.bar.compare(that.bar)      
      if (c != 0) return c
      
      return 0
    }
  }

  trait Extractors extends DefaultExtractors with ExtractionHelpers {
    implicit val DemoProductExtractor: Extractor[DemoProduct] = new Extractor[DemoProduct] {
      def extract(jvalue: JValue): DemoProduct = {
        DemoProduct(
          extractField[String](jvalue, "foo", ""),
          extractField[Int](jvalue, "bar", "")
        )
      }
    }
  }

  trait Decomposers extends DefaultDecomposers {
    implicit val DemoProductDecomposer: Decomposer[DemoProduct] = new Decomposer[DemoProduct] {
      def decompose(tvalue: DemoProduct): JValue = {
        JObject(
          JField("foo", tvalue.foo.serialize) ::
          JField("bar", tvalue.bar.serialize) ::
          Nil
        )
      }
    }
  }

  object Serialization extends SerializationImplicits with Extractors with Decomposers {
  
  }

}

}
}
