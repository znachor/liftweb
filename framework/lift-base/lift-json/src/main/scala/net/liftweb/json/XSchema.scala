package net.liftweb.json

import net.liftweb.json.JsonAST._

trait Extractor[T] {
  /**
   * Extracts the value from a JSON object.
   */
  def extract(jvalue: JValue): T
}

trait Decomposer[T] {
  /**
   * Decomposes the value into a JSON object.
   */
  def decompose(tvalue: T): JValue
}

/**
 * A serializer is capable of serializing/deserializing a wide variety of types
 * using the implicit typeclass pattern.
 */
trait Serializer {
  def serialize[T](tvalue: T)(implicit decomposer: Decomposer[T]): JValue
  
  def deserialize[T](jvalue: JValue)(implicit extractor: Extractor[T]): T
}