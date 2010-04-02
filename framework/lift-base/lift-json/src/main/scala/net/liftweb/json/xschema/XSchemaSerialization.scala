package net.liftweb.json.xschema {

object XSchemaSerialization {
  import _root_.net.liftweb.json.JsonAST._
  import _root_.net.liftweb.json.Validation._
  import XSchemaAST._
  
  private val TYPE        = "type"
  private val NAME        = "name"
  private val DOC         = "doc"
  private val NAMESPACE   = "namespace"
  private val PROPERTIES  = "properties"
  private val VERSION     = "version"
  private val DEFINITIONS = "definitions"
  private val DEFAULT     = "default"
  private val TYPEP1      = "contains"
  private val COLLECTION  = "collection"
  private val SET         = "set"
  private val ARRAY       = "array"
  private val LIST        = "list"
  private val FIELDS      = "fields"
  private val TYPES       = "types"
  private val ORDER       = "order"
  private val ASCENDING   = "ascending"
  private val DESCENDING  = "descending"
  private val IGNORE      = "ignore"

  def decompose(schema: XSchema): JValue = {
    val decomposers = List[PartialFunction[XSchema, JField]](
      { case x: Typed          => JField(TYPE,        JString(x.typename)) },
      { case x: Named          => JField(NAME,        JString(x.name)) },
      { case x: Default        => JField(DEFAULT,     x.defValue) },
      { case x: Parameterized1 => JField(TYPEP1,      decompose(x.typep1)) },
      { case x: Ordered        => JField(ORDER,       JString(x.order.name)) },
      { case x: Properties     => JField(PROPERTIES,  JObject(x.properties.map { t => JField(t._1, JString(t._2)) }.toList)) },
      { case x: Namespaced     => JField(NAMESPACE,   JString(x.namespace.value)) },
      { case x: Versioned      => JField(VERSION,     JInt(x.version)) },
      { case x: XTuple         => JField(TYPES,       JArray(x.types.map(decompose(_)))) },
      { case x: XSchemaRoot    => JField(DEFINITIONS, JArray(x.definitions.map(decompose(_)))) },
      { case x: XCollection    => JField(COLLECTION,  JString(x.collection.name)) },
      { case x: XProduct       => JField(FIELDS,      JArray(x.fields.map(decompose(_)))) },
      { case x: XCoproduct     => JField(TYPES,       JArray(x.types.map(decompose(_)))) }
    )
    
    decomposers.foldLeft[JValue](JObject(Nil)) { (cur, d) => if (d.isDefinedAt(schema)) cur ++ d(schema) else cur }
  }
  
  def extract(json: JValue): XSchema = {
    def extract0(json: JPathValue): XSchema = {
      def extractArray(s: String): List[XSchema] = arrayFieldMap(s, json, extract0)
      
      def extractArrayOf[T <: XSchema](s: String, c: Class[T]): List[T] = extractArray(s).filter {
        x => c.isAssignableFrom(x.getClass) match {
          case true  => true
          case false => throw ValidationError("Expected elements in ${path} array to extract to " + c.toString() + ", but found: " + x.getClass, json ^ s)
        }
      }.map {
        x => x.asInstanceOf[T]
      }
    
      def typename = stringField(TYPE, json)
      
      def name = stringField(NAME, json)
      
      def defValue = json \ DEFAULT #= classOf[JObject]
      
      def typep1: XSchemaReference = extract0(json \ TYPEP1) match {
        case x: XSchemaReference => x
        case x @ _ => throw ValidationError("Expected XSchemaReference but found: " + x, json ^ TYPEP1)
      }

      def order = stringField(ORDER, json) match { 
        case Ascending.name  => Ascending
        case Descending.name => Descending
        case Ignore.name     => Ignore
      }
      
      def properties = (json \? PROPERTIES).map { props => 
        Map(
          (props #= classOf[JObject]).values.map {
            case (k, JString(v)) => (k, v)
            case (k, v) => throw ValidationError("Expected string value but found: " + v, json ^ PROPERTIES)
          }.toList: _*
        )
      }.getOrElse(Map())
      
      def namespace = Namespace(stringField(NAMESPACE, json))
      
      def collection = stringField(COLLECTION, json) match {
        case XSet.name   => XSet
        case XArray.name => XArray
        case XList.name  => XList
      }
      
      def fields = extractArrayOf(FIELDS, classOf[XField])
      
      def types = extractArrayOf(TYPES, classOf[XSchemaReference])
      
      def definitions = extractArrayOf(DEFINITIONS, classOf[XSchemaDefinition])
      
      def version = integerField(VERSION, json).intValue
    
      typename match {
        case XCollection.typename => XCollection(typep1, collection)
        case XConstant.typename   => XConstant(typep1, defValue)
        case XMap.typename        => XMap(typep1)
        case XOptional.typename   => XOptional(typep1)
        case XTuple.typename      => XTuple(types)
        case XField.typename      => XField(typep1, name, properties, defValue, order)
        case XProduct.typename    => XProduct(namespace, name, properties, fields)
        case XCoproduct.typename  => XCoproduct(namespace, name, properties, types)
        case XSchemaRoot.typename => XSchemaRoot(version, definitions, properties)
        
        case _ => XSchemaReference(typename)
      }
    }
    
    extract0(!json)
  }
}

}