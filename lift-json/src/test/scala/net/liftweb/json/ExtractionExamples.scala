package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class ExtractionExampleTest extends Runner(ExtractionExamples) with JUnit
object ExtractionExamples extends Specification {
  import JsonAST._
  import JsonDSL._
  import JsonParser._

  "Extraction example" in {
    val json = parse(testJson)
    json.extract[Person] mustEqual Person("joe", Address("Bulevard", "Helsinki"), List(Child("Mary", 5), Child("Mazy", 3)))
  }

  "Partial extraction example" in {
    val json = parse(testJson)
    json.extract[SimplePerson] mustEqual SimplePerson("joe", Address("Bulevard", "Helsinki"))
  }

  "Primitive extraction example" in {
    val json = parse(primitives)
    json.extract[Primitives] mustEqual Primitives(124, 123L, 126.5, 127.5.floatValue, "128", 125, 129.byteValue, true)
  }

  val testJson = 
"""
{ "name": "joe",
  "address": {
    "street": "Bulevard",
    "city": "Helsinki"
  },
  "children": [
    {
      "name": "Mary",
      "age": 5
    },
    {
      "name": "Mazy",
      "age": 3
    }
  ]
}
"""

  val primitives = 
"""
{
  "l": 123,
  "i": 124,
  "sh": 125,
  "d": 126.5,
  "f": 127.5,
  "s": "128",
  "b": 129,
  "bool": true
}
"""
}

case class Person(name: String, address: Address, children: List[Child])
case class Address(street: String, city: String)
case class Child(name: String, age: Int)

case class SimplePerson(name: String, address: Address)

case class Primitives(i: Int, l: Long, d: Double, f: Float, s: String, sh: Short, b: Byte, bool: Boolean)
