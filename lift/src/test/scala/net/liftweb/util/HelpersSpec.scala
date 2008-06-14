package net.liftweb.util

import org.specs.Specification
import org.specs.runner._

import Helpers._

class HelpersTest extends Runner(HelpersSpec) with JUnit

object HelpersSpec extends Specification {
  "paramsToUrlParams" should {
    "handle the empty case" in {
      paramsToUrlParams(Nil) mustBe ""
    }
  }
}
