package net.liftweb.json.xschema {

import net.liftweb.json.JsonAST._

object XSchemaAST {
  case class Namespace(value: String) {
    def path = value.split(".").filter(_.length > 0)
  }
  
  sealed trait Named      { val name: String }  
  sealed trait Namespaced { val namespace: Namespace }  
  sealed trait Default    { val defValue: JValue }  
  sealed trait Typed      { val typename: String }  
  sealed trait Ordered    { val order: Order }  
  sealed trait Versioned  { val version: Int }
  sealed trait Properties { val properties: Map[String, String] }  
  sealed trait Container  { def elements: List[XSchema] }
  
  sealed trait Parameterized1 extends Container { 
    val typep1: XSchemaReference;
    
    def elements = List(typep1)
  }
  
  sealed abstract class TypeNamed(val typename: String)
  
  sealed abstract class Order(val name: String) extends Named

  case object Ascending  extends Order("ascending")
  case object Descending extends Order("descending")
  case object Ignore     extends Order("ignore")

  sealed abstract class XCollectionType(val name: String) extends Named

  case object XSet   extends XCollectionType("set")
  case object XArray extends XCollectionType("array")
  case object XList  extends XCollectionType("list")

  /** All schemas are one of: root, reference to a defined type, type definition introducing a new type, field definition, or constant */
  sealed trait XSchema extends Typed

  sealed case class XSchemaRoot(version: Int, definitions: List[XSchemaDefinition], properties: Map[String, String]) extends TypeNamed(XSchemaRoot.typename) with XSchema with Properties with Versioned
  
  sealed class XSchemaReference protected (val typename: String) extends XSchema {
    override def hashCode = typename.hashCode
    
    override def equals(a: Any) = a match {
      case x: XSchemaReference => this.typename == x.typename
      case _ => false
    }
    
    override def toString = "XSchemaReference(" + typename + ")"
  }
  
  sealed abstract class XSchemaDefinition(val typename: String) extends XSchema with Properties with Named with Namespaced with Container {
    def qualifiedName = namespace.value + "." + name
  }
  
  sealed abstract class XFieldDefinition(val typename: String) extends XSchema with Properties with Named with Default with Parameterized1 with Ordered
  
  sealed case class XConstant(typep1: XSchemaReference, defValue: JValue) extends XSchemaReference(XConstant.typename) with Default with Parameterized1 
  
  case object XString  extends XSchemaReference("String")
  case object XInt     extends XSchemaReference("Int")
  case object XLong    extends XSchemaReference("Long")
  case object XFloat   extends XSchemaReference("Float")
  case object XDouble  extends XSchemaReference("Double")
  case object XBoolean extends XSchemaReference("Boolean")

  case class XOptional(typep1: XSchemaReference) extends XSchemaReference(XOptional.typename) with Parameterized1 with Container
  
  case class XCollection(typep1: XSchemaReference, collection: XCollectionType) extends XSchemaReference(XCollection.typename) with Parameterized1

  case class XMap(typep1: XSchemaReference) extends XSchemaReference(XMap.typename) with Parameterized1

  case class XTuple(types: List[XSchemaReference]) extends XSchemaReference(XTuple.typename) with Container {
    def arity = types.length
    
    def elements = types
  }

  case class XField(typep1: XSchemaReference, name: String, properties: Map[String, String], defValue: JValue, order: Order) extends XFieldDefinition(XField.typename) 
  
  case class XProduct(namespace: Namespace, name: String, properties: Map[String, String], fields: List[XField]) extends XSchemaDefinition(XProduct.typename) {
    def elements = fields
  }
  
  case class XCoproduct(namespace: Namespace, name: String, properties: Map[String, String], types: List[XSchemaReference]) extends XSchemaDefinition(XCoproduct.typename) {
    def elements = types
  }

  object XSchemaReference {
    def apply(typename: String): XSchemaReference = typename match {
      case XString.typename     => XString
      case XInt.typename        => XInt
      case XLong.typename       => XLong
      case XFloat.typename      => XFloat
      case XDouble.typename     => XDouble
      case XBoolean.typename    => XBoolean
      
      case _ => new XSchemaReference(typename)
    }
  }
  
  object XSchemaRoot extends TypeNamed("root")
  object XOptional   extends TypeNamed("optional")
  object XCollection extends TypeNamed("collection")
  object XConstant   extends TypeNamed("constant")
  object XMap        extends TypeNamed("map")
  object XTuple      extends TypeNamed("tuple")
  object XField      extends TypeNamed("field")
  object XProduct    extends TypeNamed("product")
  object XCoproduct  extends TypeNamed("coproduct")
}

}