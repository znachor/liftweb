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
  
  "validSuffixes" should {
    "contain all its suffixes" in {
      validSuffixes mustContain "png"
      validSuffixes mustContain "js"
      validSuffixes mustContain "css"
      validSuffixes mustContain "jpg"
      validSuffixes mustContain "ico"
      validSuffixes mustContain "gif"
      validSuffixes mustContain "tiff"
      validSuffixes mustContain "jpeg"
    }
  }
  
  "first_?" should {
    val l = List("a", "b", "c", "d", "e")
    
    "return the first element if f is always true" in {
      first_?(l)(x => true) mustEqual Full("a")
    }
    
    "return empty if f is always false" in {
      first_?(l)(x => false) mustEqual Empty
    }
    
    "not invoke unnecessary side-effects" in {
      val f = (x: String) =>
        if (x == "a")
          true
        else
          throw new Exception("Unnecessary side-effect")
      
      first_?(l)(f) mustEqual Full("a")
    }
  }
  
  "first" should {
    val l = List("a", "b", "c", "d", "e")
    
    "return the first element if f is always full" in {
      first(l)(x => Full(x.toUpperCase)) mustEqual Full("A")
    }
    
    "return empty if f is always empty" in {
      first(l)(x => Empty) mustEqual Empty
    }
    
    "not invoke unnecessary side-effects" in {
      val f = (x: String) =>
        if (x == "a")
          Full(x.toUpperCase)
        else
          throw new Exception("Unnecessary side-effect")
        
      first(l)(f) mustEqual Full("A")
    }
  }
}
