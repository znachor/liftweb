package net.liftweb.json.xschema {

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
  implicit val jvalueExtractor: Extractor[JValue] = new Extractor[JValue] {
    def extract(jvalue: JValue): JValue = jvalue
  }
  
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
  
  implicit def optionExtractor[T](implicit extractor: Extractor[T]): Extractor[Option[T]] = new Extractor[Option[T]] {
    def extract(jvalue: JValue): Option[T] = jvalue match {
      case JNothing | JNull => None
      case x: JValue => Some(extractor.extract(x))
    }
  }
  
  implicit def tuple2Extractor[T1, T2](implicit extractor1: Extractor[T1], extractor2: Extractor[T2]): Extractor[(T1, T2)] = new Extractor[(T1, T2)] {
    def extract(jvalue: JValue): (T1, T2) = jvalue match {
      case JArray(values) if (values.length == 2) => (extractor1(values(0)), extractor2(values(1)))

      case _ => error("Expected array of length 2 but found: " + jvalue)
    }
  }
  
  implicit def tuple3Extractor[T1, T2, T3](implicit extractor1: Extractor[T1], extractor2: Extractor[T2], extractor3: Extractor[T3]): Extractor[(T1, T2, T3)] = new Extractor[(T1, T2, T3)] {
    def extract(jvalue: JValue): (T1, T2, T3) = jvalue match {
      case JArray(values) if (values.length == 3) => (extractor1(values(0)), extractor2(values(1)), extractor3(values(2)))

      case _ => error("Expected array of length 3 but found: " + jvalue)
    }
  }
  
  implicit def tuple4Extractor[T1, T2, T3, T4](implicit extractor1: Extractor[T1], extractor2: Extractor[T2], extractor3: Extractor[T3], extractor4: Extractor[T4]): Extractor[(T1, T2, T3, T4)] = new Extractor[(T1, T2, T3, T4)] {
    def extract(jvalue: JValue): (T1, T2, T3, T4) = jvalue match {
      case JArray(values) if (values.length == 4) => (extractor1(values(0)), extractor2(values(1)), extractor3(values(2)), extractor4(values(3)))

      case _ => error("Expected array of length 4 but found: " + jvalue)
    }
  }
  
  implicit def tuple5Extractor[T1, T2, T3, T4, T5](implicit extractor1: Extractor[T1], extractor2: Extractor[T2], extractor3: Extractor[T3], extractor4: Extractor[T4], extractor5: Extractor[T5]): Extractor[(T1, T2, T3, T4, T5)] = new Extractor[(T1, T2, T3, T4, T5)] {
    def extract(jvalue: JValue): (T1, T2, T3, T4, T5) = jvalue match {
      case JArray(values) if (values.length == 5) => (extractor1(values(0)), extractor2(values(1)), extractor3(values(2)), extractor4(values(3)), extractor5(values(4)))

      case _ => error("Expected array of length 5 but found: " + jvalue)
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
  
  implicit def mapExtractor[K, V](implicit keyExtractor: Extractor[K], valueExtractor: Extractor[V]): Extractor[Map[K, V]] = new Extractor[Map[K, V]] {
    def extract(jvalue: JValue): Map[K, V] = Map(listExtractor(tuple2Extractor(keyExtractor, valueExtractor)).extract(jvalue): _*)
  }
}

trait ExtractionHelpers extends SerializationImplicits {
  import JsonParser._
  
  protected def extractField[T](jvalue: JValue, name: String, default: JValue)(implicit e: Extractor[T]): T = {
    try {
      (jvalue \ name -->? classOf[JField]).map(_.value).getOrElse(default).deserialize[T]
    }
    catch {
      case _ => default.deserialize[T]
    }
  }
}

/**
 * Decomposers for all basic types.
 */
trait DefaultDecomposers {
  implicit val jvalueDecomposer: Decomposer[JValue] = new Decomposer[JValue] {
    def decompose(tvalue: JValue): JValue = tvalue
  }
  
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
  
  implicit def optionDecomposer[T](implicit decomposer: Decomposer[T]): Decomposer[Option[T]] = new Decomposer[Option[T]] {
    def decompose(tvalue: Option[T]): JValue = tvalue match {
      case None    => JNull
      case Some(v) => decomposer.decompose(v)
    }
  }
  
  implicit def tuple2Decomposer[T1, T2](implicit decomposer1: Decomposer[T1], decomposer2: Decomposer[T2]): Decomposer[(T1, T2)] = new Decomposer[(T1, T2)] {
    def decompose(tvalue: (T1, T2)) = JArray(decomposer1(tvalue._1) :: decomposer2(tvalue._2) :: Nil)
  }
  
  implicit def tuple3Decomposer[T1, T2, T3](implicit decomposer1: Decomposer[T1], decomposer2: Decomposer[T2], decomposer3: Decomposer[T3]): Decomposer[(T1, T2, T3)] = new Decomposer[(T1, T2, T3)] {
    def decompose(tvalue: (T1, T2, T3)) = JArray(decomposer1(tvalue._1) :: decomposer2(tvalue._2) :: decomposer3(tvalue._3) :: Nil)
  }
  
  implicit def tuple4Decomposer[T1, T2, T3, T4](implicit decomposer1: Decomposer[T1], decomposer2: Decomposer[T2], decomposer3: Decomposer[T3], decomposer4: Decomposer[T4]): Decomposer[(T1, T2, T3, T4)] = new Decomposer[(T1, T2, T3, T4)] {
    def decompose(tvalue: (T1, T2, T3, T4)) = JArray(decomposer1(tvalue._1) :: decomposer2(tvalue._2) :: decomposer3(tvalue._3) :: decomposer4(tvalue._4) :: Nil)
  }
  
  implicit def tuple5Decomposer[T1, T2, T3, T4, T5](implicit decomposer1: Decomposer[T1], decomposer2: Decomposer[T2], decomposer3: Decomposer[T3], decomposer4: Decomposer[T4], decomposer5: Decomposer[T5]): Decomposer[(T1, T2, T3, T4, T5)] = new Decomposer[(T1, T2, T3, T4, T5)] {
    def decompose(tvalue: (T1, T2, T3, T4, T5)) = JArray(decomposer1(tvalue._1) :: decomposer2(tvalue._2) :: decomposer3(tvalue._3) :: decomposer4(tvalue._4) :: decomposer5(tvalue._5) :: Nil)
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
  
  implicit def mapDecomposer[K, V](implicit keyDecomposer: Decomposer[K], valueDecomposer: Decomposer[V]): Decomposer[Map[K, V]] = new Decomposer[Map[K, V]] {
    def decompose(tvalue: Map[K, V]): JValue = listDecomposer(tuple2Decomposer(keyDecomposer, valueDecomposer)).decompose(tvalue.toList)
  }
}

trait DecomposerHelpers extends SerializationImplicits {
}

trait DefaultOrderings {
  implicit def OptionToOrderedOption[T <% Ordered[T]](opt: Option[T]): OrderedOption[T] = OrderedOption[T](opt)
  implicit def ArrayToOrderedArray[T <% Ordered[T]](c: Array[T]): OrderedArray[T] = OrderedArray[T](c)
  implicit def ListToOrderedList[T <% Ordered[T]](c: List[T]): OrderedList[T] = OrderedList[T](c)
  implicit def SetToOrderedSet[T <% Ordered[T]](c: Set[T]): OrderedSet[T] = OrderedSet[T](c)
  implicit def JValueToOrderedJValue(jvalue: JValue): OrderedJValue = OrderedJValue(jvalue)
  implicit def JFieldToOrderedJField(jfield: JField): OrderedJField = OrderedJField(jfield)
  
  case class OrderedOption[T <% Ordered[T]](opt: Option[T]) extends Ordered[Option[T]] {
    def compare(that: Option[T]): Int = {
      if (opt.isEmpty && that.isEmpty) return 0
      if (opt.isEmpty && !that.isEmpty) return -1
      if (!opt.isEmpty && that.isEmpty) return 1
      
      return opt.get.compareTo(that.get)
    }
  }
  
  case class OrderedArray[T <% Ordered[T]](col: Array[T]) extends Ordered[Array[T]] {
    def compare(that: Array[T]): Int = {
      col.zip(that).toList.map(t => t._1.compare(t._2)).dropWhile(_ == 0).firstOption match {
        case None => col.length.compare(that.length)
        
        case Some(c: Int) => c
      }
    }
  }
  
  case class OrderedList[T <% Ordered[T]](col: List[T]) extends Ordered[List[T]] {
    def compare(that: List[T]): Int = {
      col.zip(that).map(t => t._1.compare(t._2)).dropWhile(_ == 0).firstOption match {
        case None => col.length.compare(that.length)
        
        case Some(c: Int) => c
      }
    }
  }
  
  case class OrderedSet[T <% Ordered[T]](col: Set[T]) extends Ordered[Set[T]] {
    def compare(that: Set[T]): Int = col.toList.sort(_.compare(_) < 0).compareTo(that.toList.sort(_.compare(_) < 0))
  }
  
  case class OrderedJField(jfield: JField) extends Ordered[JField] {
    def compare(that: JField): Int = {
      var c = jfield.name.compare(that.name)
      if (c != 0) return c
      
      c = jfield.value.compare(that.value)
      
      return c
    }
  }
  
  case class OrderedJValue(jvalue: JValue) extends Ordered[JValue] {
    def compare(that: JValue): Int = jvalue match {
      case JNothing => that match {
        case JNothing => 0
        case _ => -1
      }
      case JNull => that match {
        case JNothing => 1
        case JNull => 0
        case _ => -1
      }
      case JBool(v1) => that match {
        case JNothing | JNull => 1
        case JBool(v2) => v1.compare(v2)
        case _ => -1
      }
      case JInt(v1) => that match {
        case JNothing | JNull | JBool(_) => 1
        case JInt(v2) => v1.compare(v2)
        case _ => -1
      }
      case JDouble(v1) => that match {
        case JNothing | JNull | JBool(_) | JInt(_) => 1
        case JDouble(v2) => v1.compare(v2)
        case _ => -1
      }
      case JString(v1) => that match {
        case JNothing | JNull | JBool(_) | JInt(_) | JDouble(_) => 1
        case JString(v2) => v1.compare(v2)
        case _ => -1
      }
      case JField(_, _) => jvalue.compare(that)
      case JObject(v1) => that match {
        case JNothing | JNull | JBool(_) | JInt(_) | JDouble(_) | JString(_) | JField(_, _) => 1
        case JObject(v2) => v1.compare(v2)
        case _ => -1
      }
      case JArray(v1) => that match {
        case JNothing | JNull | JBool(_) | JInt(_) | JDouble(_) | JString(_) | JField(_, _) | JObject(_) => 1
        case JArray(v2) => v1.compare(v2)
        case _ => -1
      }
    }
  }
}

object DefaultSerialization extends SerializationImplicits with DefaultExtractors with DefaultDecomposers with DefaultOrderings {
}

}