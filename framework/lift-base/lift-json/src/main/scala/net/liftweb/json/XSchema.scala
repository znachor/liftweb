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

trait SerializationImplicits {
  case class DeserializableJValue(jvalue: JValue) {
    def deserialize[T](implicit e: Extractor[T]): T = e(jvalue)
  }
  case class SerializableTValue[T](tvalue: T) {
    def serialize(implicit d: Decomposer[T]): JValue = d(tvalue)
  }
  
  implicit def jvalueToTValue[T](jvalue: JValue): DeserializableJValue = DeserializableJValue(jvalue)
  
  implicit def tvalueToJValue[T](tvalue: T): SerializableTValue[T] = SerializableTValue[T](tvalue)
}

/**
 * Extractors for all basic types.
 */
trait DefaultExtractors {
  implicit val stringExtractor: Extractor[String] = new Extractor[String] {
    def extract(jvalue: JValue): String = jvalue match {
      case JString(str) => str
      case _ => error("Expected string but found: " + jvalue)
    }
  }
  
  implicit val booleanExtractor: Extractor[Boolean] = new Extractor[Boolean] {
    def extract(jvalue: JValue): Boolean = jvalue match {
      case JBool(b) => b
      
      case JString(s) if (s.toLowerCase == "true")  => true
      case JString(s) if (s.toLowerCase == "false") => false
      
      case JString(s) if (s.toLowerCase == "1") => true
      case JString(s) if (s.toLowerCase == "0") => false
      
      case JInt(i) if (i.intValue == 1) => true
      case JInt(i) if (i.intValue == 0) => false
      
      case _ => error("Expected boolean but found: " + jvalue)
    }
  }
  
  implicit val intExtractor: Extractor[Int] = new Extractor[Int] {
    def extract(jvalue: JValue): Int = jvalue match {
      case JInt(i)    => i.intValue
      case JDouble(d) => d.toInt
      
      case JString(s) => s.toInt
      
      case _ => error("Expected integer but found: " + jvalue)
    }
  }
  
  implicit val longExtractor: Extractor[Long] = new Extractor[Long] {
    def extract(jvalue: JValue): Long = jvalue match {
      case JInt(i)    => i.longValue
      case JDouble(d) => d.toLong
      
      case JString(s) => s.toLong
      
      case _ => error("Expected long but found: " + jvalue)
    }
  }
  
  implicit val floatExtractor: Extractor[Float] = new Extractor[Float] {
    def extract(jvalue: JValue): Float = jvalue match {
      case JInt(i)    => i.floatValue
      case JDouble(d) => d.toFloat
      
      case JString(s) => s.toFloat
      
      case _ => error("Expected float but found: " + jvalue)
    }
  }

  implicit val doubleExtractor: Extractor[Double] = new Extractor[Double] {
    def extract(jvalue: JValue): Double = jvalue match {
      case JInt(i)    => i.doubleValue
      case JDouble(d) => d
      
      case JString(s) => s.toDouble

      case _ => error("Expected double but found: " + jvalue)
    }
  }
  
  implicit def arrayExtractor[T](implicit elementExtractor: Extractor[T]): Extractor[Array[T]] = new Extractor[Array[T]] {
    def extract(jvalue: JValue): Array[T] = jvalue match {
      case JArray(values) => values.map(elementExtractor.extract _).toArray

      case _ => error("Expected array but found: " + jvalue)
    }
  }
  
  implicit def setExtractor[T](implicit elementExtractor: Extractor[T]): Extractor[Set[T]] = new Extractor[Set[T]] {
    def extract(jvalue: JValue): Set[T] = jvalue match {
      case JArray(values) => Set(values.map(elementExtractor.extract _): _*)

      case _ => error("Expected set but found: " + jvalue)
    }
  }
  
  implicit def listExtractor[T](implicit elementExtractor: Extractor[T]): Extractor[List[T]] = new Extractor[List[T]] {
    def extract(jvalue: JValue): List[T] = jvalue match {
      case JArray(values) => values.map(elementExtractor.extract _)

      case _ => error("Expected list but found: " + jvalue)
    }
  }
}

/**
 * Decomposers for all basic types.
 */
trait DefaultDecomposers {
  implicit val stringDecomposer: Decomposer[String] = new Decomposer[String] {
    def decompose(tvalue: String): JValue = JString(tvalue)
  }
  
  implicit val booleanDecomposer: Decomposer[Boolean] = new Decomposer[Boolean] {
    def decompose(tvalue: Boolean): JValue = JBool(tvalue)
  }
  
  implicit val intDecomposer: Decomposer[Int] = new Decomposer[Int] {
    def decompose(tvalue: Int): JValue = JInt(BigInt(tvalue))
  }
  
  implicit val longDecomposer: Decomposer[Long] = new Decomposer[Long] {
    def decompose(tvalue: Long): JValue = JInt(BigInt(tvalue))
  }
  
  implicit val floatDecomposer: Decomposer[Float] = new Decomposer[Float] {
    def decompose(tvalue: Float): JValue = JDouble(tvalue.toDouble)
  }

  implicit val doubleDecomposer: Decomposer[Double] = new Decomposer[Double] {
    def decompose(tvalue: Double): JValue = JDouble(tvalue)
  }
  
  implicit def arrayDecomposer[T](implicit elementDecomposer: Decomposer[T]): Decomposer[Array[T]] = new Decomposer[Array[T]] {
    def decompose(tvalue: Array[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
  
  implicit def setDecomposer[T](implicit elementDecomposer: Decomposer[T]): Decomposer[Set[T]] = new Decomposer[Set[T]] {
    def decompose(tvalue: Set[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
  
  implicit def listDecomposer[T](implicit elementDecomposer: Decomposer[T]): Decomposer[List[T]] = new Decomposer[List[T]] {
    def decompose(tvalue: List[T]): JValue = JArray(tvalue.toList.map(elementDecomposer.decompose _))
  }
}

object XSchema extends SerializationImplicits with DefaultExtractors with DefaultDecomposers {
}

