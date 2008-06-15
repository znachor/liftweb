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
  }

  "appendParams" should {
    val basicUrl = "http://example.com/index.html"
    val queryUrl = basicUrl + "?first=param"
    
    "handle no parameters" in {
      appendParams(basicUrl, List()) mustEqual basicUrl
      appendParams(queryUrl, List()) mustEqual queryUrl
    }
    
    "handle parameters with a basic url" in {
      appendParams(basicUrl, List(("foo", "bar"))) mustEqual
        (basicUrl + "?foo=bar")
      appendParams(basicUrl, List(("foo", "bar"), ("fooz", "baz"))) mustEqual
        (basicUrl + "?foo=bar&fooz=baz")
    }
    
    "handle parameters with a query url" in {
      appendParams(queryUrl, List(("foo", "bar"))) mustEqual
        (queryUrl + "&foo=bar")
      appendParams(queryUrl, List(("foo", "bar"), ("fooz", "baz"))) mustEqual
        (queryUrl + "&foo=bar&fooz=baz")      
    }
  }
}
