package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class ExampleTest extends Runner(Examples) with JUnit
object Examples extends Specification {
  import JsonAST._
  import JsonDSL._
  import JsonParser._

  "Lotto example" in {
    val json = parse(lotto)
    val renderedLotto = compact(render(json))
    json mustEqual parse(renderedLotto)
  }

  "Person example" in {
    val json = parse(person)
    val renderedPerson = JsonDSL.pretty(render(json))
    json mustEqual parse(renderedPerson)
    render(json) mustEqual render(personDSL)
    compact(render(json \\ "name")) mustEqual """{"name":"Joe","name":"Marilyn"}"""
    compact(render(json \ "person" \ "name")) mustEqual "\"name\":\"Joe\""
  }

  "Object array example" in {
    val json = parse(objArray)
    compact(render(json \ "children" \ "name")) mustEqual """["name":"Mary","name":"Mazy"]"""
    compact(render((json \ "children")(0) \ "name")) mustEqual "\"name\":\"Mary\""
    compact(render((json \ "children")(1) \ "name")) mustEqual "\"name\":\"Mazy\""
  }

  "Quoted example" in {
    val json = parse(quoted)
    List("foo \" \n \t \r bar") mustEqual json.values
  }

  "Extraction example" in {
    val json = parse(objArray)
    json.extract[Person] mustEqual Person("joe", Address("Bulevard", "Helsinki"), List(Child("Mary", BigInt(5)), Child("Mazy", BigInt(3))))
  }

  "Partial extraction example" in {
    val json = parse(objArray)
    json.extract[SimplePerson] mustEqual SimplePerson("joe", Address("Bulevard", "Helsinki"))
  }

  "Null example" in {
    compact(render(parse(""" {"name": null} """))) mustEqual """{"name":null}"""
  }

  val lotto = """
{
  "lotto":{
    "lotto-id":5,
    "winning-numbers":[2,45,34,23,7,5,3],
    "winners":[ {
      "winner-id":23,
      "numbers":[2,45,34,23,3, 5]
    },{
      "winner-id" : 54 ,
      "numbers":[ 52,3, 12,11,18,22 ]
    }]
  }
}
"""

  val person = """
{ 
  "person": {
    "name": "Joe",
    "age": 35,
    "spouse": {
      "person": {
        "name": "Marilyn",
        "age": 33
      }
    }
  }
}
"""

  val personDSL = 
    ("person" ->
      ("name" -> "Joe") ~
      ("age" -> 35) ~
      ("spouse" -> 
        ("person" -> 
          ("name" -> "Marilyn") ~
          ("age" -> 33)
        )
      )
    )

  val objArray = 
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

  val quoted = """["foo \" \n \t \r bar"]"""
}

case class Person(name: String, address: Address, children: List[Child])
case class Address(street: String, city: String)
case class Child(name: String, age: BigInt)
//  case class Child(name: String, age: Int)

case class SimplePerson(name: String, address: Address)
