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

class DefaultSerializationExamplesTest extends Runner(DefaultSerializationExamples) with JUnit
object DefaultSerializationExamples extends Specification {
  import _root_.net.liftweb.json.JsonAST._
  
  import Serialization._
  
  "Primitives can be extracted from strings" in {
    IntExtractor(JString("12")) mustEqual 12
    LongExtractor(JString("12")) mustEqual 12    
    FloatExtractor(JString("12.5")) mustEqual 12.5F
    DoubleExtractor(JString("12.5")) mustEqual 12.5
    BooleanExtractor(JString("true")) mustEqual true
    BooleanExtractor(JString("false")) mustEqual false
    BooleanExtractor(JString("0")) mustEqual false
    BooleanExtractor(JString("1")) mustEqual true
  }
  
  "Reals can be extracted from integers" in {
    FloatExtractor(JInt(12)) mustEqual 12.0F
    DoubleExtractor(JInt(12)) mustEqual 12.0
  }
  
  "Booleans can be extracted from integers" in {
    BooleanExtractor(JInt(0)) mustEqual false
    BooleanExtractor(JInt(1)) mustEqual true
  }
  
  "Integers can be extracted from reals" in {
    IntExtractor(JDouble(12.0)) mustEqual 12
    LongExtractor(JDouble(12.0)) mustEqual 12L
  }
}


}

