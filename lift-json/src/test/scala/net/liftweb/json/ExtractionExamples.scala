package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class ExtractionExampleTest extends Runner(ExtractionExamples) with JUnit
object ExtractionExamples extends Specification {
  import JsonAST._
  import JsonParser._

  "Extraction example" in {
    val json = parse(testJson)
    json.extract[Person] mustEqual Person("joe", Address("Bulevard", "Helsinki"), List(Child("Mary", 5), Child("Mazy", 3)))
  }

  "Extraction with path expression example" in {
    val json = parse(testJson)
    (json \ "address").extract[Address] mustEqual Address("Bulevard", "Helsinki")
  }

  "Partial extraction example" in {
    val json = parse(testJson)
    json.extract[SimplePerson] mustEqual SimplePerson("joe", Address("Bulevard", "Helsinki"))
  }

  "Simple value extraction example" in {
    val json = parse(testJson)
    json.extract[Name] mustEqual Name("joe")
    (json \ "children")(0).extract[Name] mustEqual Name("Mary")
    (json \ "children")(1).extract[Name] mustEqual Name("Mazy")
  }

  "Primitive extraction example" in {
    val json = parse(primitives)
    json.extract[Primitives] mustEqual Primitives(124, 123L, 126.5, 127.5.floatValue, "128", 125, 129.byteValue, true)
  }

  "Null extraction example" in {
    val json = parse("""{ "name": null, "age": 5 }""")
    json.extract[Child] mustEqual Child(null, 5)
  }

  "Option extraction example" in {
    val json = parse("""{ "name": null, "age": 5, "mother":{"name":"Marilyn"}}""")
    json.extract[OChild] mustEqual OChild(None, 5, Some(Parent("Marilyn")), None)
  }

  /* Does not work yet.
  "List extraction example" in {
    val json = parse(testJson)
    (json \ "children").extract[List[Name]] mustEqual List("Mary", "Mazy")
  }
  */

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

case class Name(name: String)

case class Primitives(i: Int, l: Long, d: Double, f: Float, s: String, sh: Short, b: Byte, bool: Boolean)

case class OChild(name: Option[String], age: Int, mother: Option[Parent], father: Option[Parent])
case class Parent(name: String)
