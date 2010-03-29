package net.liftweb.json {

/**
 * val street = (strict(json) \ "address" \ "street" #= classOf[JString]).values
 */
object Validation {
  import _root_.net.liftweb.json.JsonAST._
  
  case class ValidationError(message: String) extends Exception(message)
  
  private def failFind[T](path: String): T = throw ValidationError("Expected to find " + path)
  private def failType[T](path: String, etype: Class[_], atype: Class[_]): T = throw ValidationError("Expected " + path + " to have type " + etype.toString + " but was: " + atype.toString)
  
  case class JPathValue(path: String, value: JValue) {
    def \ (name: String) = (value \ name) match {
      case JNothing => failFind(path + "." + name)
      case x @ _ => JPathValue(path + "." + name, x)
    }
    
    def \? (name: String): Option[JPathValue] = (value \ name) match {
      case JNothing => None
      case x @ _ => Some(JPathValue(path + "." + name, x))
    }
    
    def #= [A <: JValue](clazz: Class[A]): A = {
      def extractTyped(value: JValue) = if (value.getClass == clazz) value.asInstanceOf[A] else failType(path, clazz, value.getClass)
      
      value match {
        case JField(name, value) => extractTyped(value)
        case _ => extractTyped(value)
      }
    }
    
    def ^ (s: String) = path + "." + s
    
    def apply(i: Int) = JPathValue(path + "[" + i + "]", (this #= classOf[JArray]).arr(i))
  }
  
  def arrayFieldMap[A](name: String, json: JPathValue, f: JPathValue => A): List[A] = {
    val field = json \ name 
    val array = field #= classOf[JArray]
    
    array.values.foldLeft[(Int, List[A])]((0, Nil)) { (cur, e) =>
      (cur._1 + 1, f(field(cur._1)) :: cur._2)
    }._2.reverse
  }
  
  def stringField(name: String, json: JPathValue) = (json \ name #= classOf[JString]).values
  def numberField(name: String, json: JPathValue) = (json \ name).value match {
    case JInt(i)    => i.doubleValue
    case JDouble(d) => d
    case x @ _ => throw ValidationError("Expected " + (json ^ name) + " to be number, but was: " + x)
  }
  def integerField(name: String, json: JPathValue) = (json \ name #= classOf[JInt]).values
  def doubleField(name: String, json: JPathValue) = (json \ name #= classOf[JDouble]).values
  
  def strict(j: JValue) = new JPathValue("", j)
}

}