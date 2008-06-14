package net.liftweb.util

import org.specs.Specification
import org.specs.runner._

import Helpers._

class HelpersTest extends Runner(HelpersSpec) with JUnit

object HelpersSpec extends Specification {
  "paramsToUrlParams" should {
    "handle no parameters" in {
      paramsToUrlParams(Nil) mustEqual ""
    }
    
    "handle one parameter" in {
      paramsToUrlParams(List(("foo", "bar"))) mustEqual "foo=bar"
    }
    
    "handle two parameters" in {
      paramsToUrlParams(List(
        ("foo", "bar"), ("fooz", "baz")
      )) mustEqual "foo=bar&fooz=baz"
    }
    
    "handle many parameters" in {
      paramsToUrlParams(List(
        ("foo", "bar"), ("fooz", "baz"), ("fooy", "bay"), ("foox", "bax")
      )) mustEqual "foo=bar&fooz=baz&fooy=bay&foox=bax"
    }
    
    "handle collections" in {
      paramsToUrlParams(Map("foo" -> "bar")) mustEqual "foo=bar"
    }
  }
}
