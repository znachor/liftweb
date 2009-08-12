package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class ExampleTest extends Runner(Examples) with JUnit
object Examples extends Specification {
  import JsonAST._
  import JsonDSL._
  import JsonParser._

  "Lotto example" in {
    parse(lotto) match {
      case Right(json) =>
        val renderedLotto = compact(render(json))
        json mustEqual parse(renderedLotto).right.get
      case Left(err) => fail(err.message)
    }
  }

  "Person example" in {
    parse(person) match {
      case Right(json) =>
        val renderedPerson = JsonDSL.pretty(render(json))
        json mustEqual parse(renderedPerson).right.get
        render(json) mustEqual render(personDSL)
        compact(render(json \\ "name")) mustEqual """{"name":"Joe","name":"Marilyn"}"""
        compact(render(json \ "person" \ "name")) mustEqual "\"name\":\"Joe\""
      case Left(err) => fail(err.message)
    }
  }

  "Object array example" in {
    parse(objArray) match {
      case Right(json) =>
        compact(render(json \ "children" \ "name")) mustEqual """["name":"Mary","name":"Mazy"]"""
        compact(render((json \ "children")(0) \ "name")) mustEqual "\"name\":\"Mary\""
        compact(render((json \ "children")(1) \ "name")) mustEqual "\"name\":\"Mazy\""
      case Left(err) => fail(err.message)
    }
  }

  "Quoted example" in {
    parse(quoted) match {
      case Right(json) => List("foo \" \n \t \r bar") mustEqual json.values
      case Left(err) => fail(err.message)
    }
  }

  "Extraction example" in {
    parse(objArray) match {
      case Right(json) =>
        json.extract[Person] mustEqual Person("joe", Address("Bulevard", "Helsinki"), List(Child("Mary", BigInt(5)), Child("Mazy", BigInt(3))))
      case Left(err) => fail(err.message)
    }
  }

  "Partial extraction example" in {
    parse(objArray) match {
      case Right(json) =>
        json.extract[SimplePerson] mustEqual SimplePerson("joe", Address("Bulevard", "Helsinki"))
      case Left(err) => fail(err.message)
    }
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
        "name": "Marilyn"
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
