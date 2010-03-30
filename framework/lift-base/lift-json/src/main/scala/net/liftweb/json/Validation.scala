package net.liftweb.json {

/**
 * This object provides an easy way to validate the extraction of data from JSON.
 * <p>
 * val street = (validate(json) \ "address" \ "street" #= classOf[JString]).values
 * <p>
 * If any field is missing or does not have the appropriate type, a very descriptive
 * ValidationError will be thrown that contains the exact reason for the failure.
 */
object Validation {
  import _root_.net.liftweb.json.JsonAST._
  
  case class ValidationError(message: String, path: String) extends Exception(message.replace("${path}", path))
  
  /**
   * A bundle of a path and JValue. The path serves to track the history and is
   * used when throwing validation errors.
   */
  case class JPathValue(path: String, value: JValue) {
    /**
     * Strictly extracts a field of the specified name from this object, 
     * throwing a ValidationError as necessary.
     */
    def \ (name: String) = (value \ name) match {
      case JNothing => failFind(path + "." + name)
      case x @ _ => JPathValue(path + "." + name, x)
    }
    
    /**
     * A non-strict extraction method that returns an Option.
     */
    def \? (name: String): Option[JPathValue] = (value \ name) match {
      case JNothing => None
      case x @ _ => Some(JPathValue(path + "." + name, x))
    }
    
    /**
     * Returns the element as a JValue of the specified class, throwing a 
     * ValidationError as necessary.
     */
    def #= [A <: JValue](clazz: Class[A]): A = {
      def extractTyped(value: JValue) = if (value.getClass == clazz) value.asInstanceOf[A] else failType(path, clazz, value.getClass)
      
      value match {
        case JField(name, value) => extractTyped(value)
        case _ => extractTyped(value)
      }
    }
    
    /**
     * Returns the path of this element, plus the specified subpath.
     */
    def ^ (s: String) = path + "." + s
    
    /**
     * Returns the i-th element of this json value as an array element. If 
     * this json value is not an array, an appropriate ValidationError will be 
     * thrown.
     */
    def !! (i: Int) = JPathValue(path + "[" + i + "]", (this #= classOf[JArray]).arr(i))
    
    private def failFind[T](path: String): T = throw ValidationError("Expected to find ${path}, but it did not exist", path)
    private def failType[T](path: String, etype: Class[_], atype: Class[_]): T = throw ValidationError("Expected ${path} to have type " + etype.toString + " but was: " + atype.toString, path)
  }
  
  def arrayFieldMap[A](name: String, json: JPathValue, f: JPathValue => A): List[A] = {
    val field = json \ name 
    val array = field #= classOf[JArray]
    
    array.values.foldLeft[(Int, List[A])]((0, Nil)) { (cur, e) =>
      (cur._1 + 1, f(field !! cur._1) :: cur._2)
    }._2.reverse
  }
  
  def stringField(name: String, json: JPathValue) = (json \ name #= classOf[JString]).values
  def numberField(name: String, json: JPathValue) = (json \ name).value match {
    case JInt(i)    => i.doubleValue
    case JDouble(d) => d
    case x @ _ => throw ValidationError("Expected ${path} to be number, but was: " + x, json ^ name)
  }
  def integerField(name: String, json: JPathValue) = (json \ name #= classOf[JInt]).values
  def doubleField(name: String, json: JPathValue) = (json \ name #= classOf[JDouble]).values
  
  def validate(j: JValue) = new JPathValue("", j)
}

}