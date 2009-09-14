package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class XmlExamplesTest extends Runner(XmlExamples) with JUnit
object XmlExamples extends Specification {
  import JsonAST._
  import JsonDSL._
  import Xml._

  "Basic conversion example" in {
    val json = toJson(xml) 
    compact(render(json)) mustEqual """{"foos":{"foo":[{"id":"1","name":"Harry"},{"id":"2","name":"David"}]}}"""
  }

  "Conversion transformation example" in {
    val json = toJson(xml) map {
      case JField("id", JString(s)) => JField("id", JInt(s.toInt))
      case x => x 
    }
    compact(render(json)) mustEqual """{"foos":{"foo":[{"id":1,"name":"Harry"},{"id":2,"name":"David"}]}}"""
  }

  val xml =
  <foos>
    <foo>
      <id>1</id>
      <name>Harry</name>
    </foo>
    <foo>
      <id>2</id>
      <name>David</name>
    </foo>
  </foos> 
}
