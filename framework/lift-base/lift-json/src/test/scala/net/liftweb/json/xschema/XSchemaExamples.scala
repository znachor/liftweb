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

package net.liftweb.json.xschema {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class XSchemaExamplesTest extends Runner(XSchemaExamples) with JUnit
object XSchemaExamples extends Specification {
  import _root_.net.liftweb.json.JsonAST._
  
  import Serialization._
  
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
  
  /*
  
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
  
  "Custom schemas are valid" in {
    import data.social._
    
    Gender.xschema mustEqual Gender.xschema
    Male.xschema mustEqual Male.xschema
    Female.xschema mustEqual Female.xschema
  }*/
}


}

