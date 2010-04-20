package net.liftweb.json

import net.liftweb.json.JsonAST._

trait Extractor[T] extends Function[JValue, T] {
  /**
   * Extracts the value from a JSON object.
   */
  def extract(jvalue: JValue): T
  
  def apply(jvalue: JValue): T = extract(jvalue)
}

trait Decomposer[T] extends Function[T, JValue] {
  /**
   * Decomposes the value into a JSON object.
   */
  def decompose(tvalue: T): JValue
  
  def apply(tvalue: T): JValue = decompose(tvalue)
}

/**
 * A serializer is capable of serializing/deserializing a wide variety of types
 * using the implicit typeclass pattern.
 */
trait Serializer {
  def serialize[T](tvalue: T)(implicit decomposer: Decomposer[T]): JValue = decomposer.decompose(tvalue)
  
  def deserialize[T](jvalue: JValue)(implicit extractor: Extractor[T]): T = extractor.extract(jvalue)
}

trait DefaultExtractors {
  implicit val stringExtractor = new Extractor[String] {
    def extract(jvalue: JValue): String = jvalue match {
      case JString(str) => str
      case _ => error("Expected string but found: " + jvalue)
    }
  }
  
  implicit val booleanExtractor = new Extractor[Boolean] {
    def extract(jvalue: JValue): Boolean = jvalue match {
      case JBool(b) => b
      case _ => error("Expected boolean but found: " + jvalue)
    }
  }
  
  implicit val intExtractor = new Extractor[Int] {
    def extract(jvalue: JValue): Int = jvalue match {
      case JInt(i) => i.intValue
      
      case _ => error("Expected integer but found: " + jvalue)
    }
  }
  
  implicit val longExtractor = new Extractor[Long] {
    def extract(jvalue: JValue): Long = jvalue match {
      case JInt(i) => i.longValue
      
      case _ => error("Expected long but found: " + jvalue)
    }
  }
  
  implicit val floatExtractor = new Extractor[Float] {
    def extract(jvalue: JValue): Float = jvalue match {
      case JDouble(d) => d.toFloat
      
      case _ => error("Expected float but found: " + jvalue)
    }
  }

  implicit val doubleExtractor = new Extractor[Double] {
    def extract(jvalue: JValue): Double = jvalue match {
      case JDouble(d) => d

      case _ => error("Expected double but found: " + jvalue)
    }
  }
  
  def arrayExtractor[T](elementExtractor: Extractor[T]) = new Extractor[Array[T]] {
    def extract(jvalue: JValue): Array[T] = jvalue match {
      case JArray(values) => values.map(elementExtractor.extract _).toArray

      case _ => error("Expected array but found: " + jvalue)
    }
  }
  
  def setExtractor[T](elementExtractor: Extractor[T]) = new Extractor[Set[T]] {
    def extract(jvalue: JValue): Set[T] = jvalue match {
      case JArray(values) => Set(values.map(elementExtractor.extract _): _*)

      case _ => error("Expected set but found: " + jvalue)
    }
  }
  
  def listExtractor[T](elementExtractor: Extractor[T]) = new Extractor[List[T]] {
    def extract(jvalue: JValue): List[T] = jvalue match {
      case JArray(values) => values.map(elementExtractor.extract _)

      case _ => error("Expected list but found: " + jvalue)
    }
  }
}

trait DefaultDecomposers {
  implicit val stringDecomposer = new Decomposer[String] {
    def decompose(tvalue: String): JValue = JString(tvalue)
  }
  
  implicit val booleanDecomposer = new Decomposer[Boolean] {
    def decompose(tvalue: Boolean): JValue = JBool(tvalue)
  }
  
  implicit val intDecomposer = new Decomposer[Int] {
    def decompose(tvalue: Int): JValue = JInt(BigInt(tvalue))
  }
  
  implicit val longDecomposer = new Decomposer[Long] {
    def decompose(tvalue: Long): JValue = JInt(BigInt(tvalue))
  }
  
  implicit val floatDecomposer = new Decomposer[Float] {
    def decompose(tvalue: Float): JValue = JDouble(tvalue.toDouble)
  }

  implicit val doubleDecomposer = new Decomposer[Double] {
    def decompose(tvalue: Double): JValue = JDouble(tvalue)
  }
  
  def arrayDecomposer[T](elementDecomposer: Decomposer[T]) = new Decomposer[Array[T]] {
    def decompose(tvalue: Array[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
  
  def setDecomposer[T](elementDecomposer: Decomposer[T]) = new Decomposer[Set[T]] {
    def decompose(tvalue: Set[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
  
  def listDecomposer[T](elementDecomposer: Decomposer[T]) = new Decomposer[List[T]] {
    def decompose(tvalue: List[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
}

object XSchema extends Serializer with DefaultExtractors with DefaultDecomposers {
}

