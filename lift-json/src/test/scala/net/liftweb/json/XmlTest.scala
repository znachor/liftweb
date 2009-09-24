package net.liftweb.json

import _root_.org.scalacheck._
import _root_.org.scalacheck.Prop.forAll
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.ScalaCheck

class XmlTest extends Runner(XmlSpec) with JUnit
object XmlSpec extends Specification with NodeGen with ScalaCheck {
  import Xml._
  import scala.xml.Node

  "Valid XML can be converted to JSON and back" in {
    val conversion = (xml: Node) => { toXml(toJson(xml)) == xml }
    forAll(conversion) must pass
  }

  implicit def arbXml: Arbitrary[Node] = Arbitrary(genXml)
}
