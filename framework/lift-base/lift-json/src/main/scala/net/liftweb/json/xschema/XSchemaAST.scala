package net.liftweb.json.xschema {

import net.liftweb.json.JsonAST._

object XSchemaAST {
  object PredefinedProperties {
    val XSchemaDoc = "xschema.doc"
  }
  
  case class Namespace(value: String) {
    def path = value.split("[.]").filter(_.length > 0)
  }
  
  sealed trait Named      { val name: String }  
  sealed trait Namespaced { val namespace: Namespace }  
  sealed trait Default    { val defValue: JValue }  
  sealed trait Typed      { val typename: String }  
  sealed trait Ordered    { val order: Order }  
  sealed trait Versioned  { val version: Int }
  sealed trait Properties { val properties: Map[String, String] }  
  sealed trait Container  { def elements: List[XSchema] }
  
  sealed trait Parameterized extends Container { 
    val typeParameters: List[XReference]
    
    def elements = typeParameters
  }
  
  sealed abstract class TypeNamed(val typename: String)
  
  sealed abstract class Order(val name: String) extends Named

  case object Ascending  extends Order("ascending")
  case object Descending extends Order("descending")
  case object Ignore     extends Order("ignore")

  sealed abstract class XCollectionType(val name: String) extends Named

  case object XSet   extends XCollectionType("Set")
  case object XArray extends XCollectionType("Array")
  case object XList  extends XCollectionType("List")

  /** All schemas are one of: root, reference, type definition, or field definition */
  sealed trait XSchema extends Typed

  sealed case class XRoot(definitions: List[XDefinition], properties: Map[String, String]) extends TypeNamed(XRoot.typename) with XSchema with Properties with Container {
    def elements = definitions
  }
  
  sealed class XReference protected (val typename: String) extends XSchema with Named with Namespaced {
    val namespace = Namespace(if (typename.indexOf('.') == -1) "" else typename.split("[.]").toList.dropRight(1).mkString("."))
    
    val name = if (typename.indexOf('.') == -1) typename else typename.split("[.]").last
    
    override def hashCode = typename.hashCode
    
    override def equals(a: Any) = a match {
      case x: XReference => this.typename == x.typename
      case _ => false
    }
    
    override def toString = "XReference(" + typename + ")"
  }
  
  sealed class XPrimitive protected (typename: String) extends XReference(typename) {
    val typeParameters = Nil
  }
  
  sealed abstract class XDefinition(val typename: String) extends XSchema with Properties with Named with Namespaced with Container {
    def qualifiedName = namespace.value + "." + name
  }
  
  case object XString  extends XPrimitive("String")
  case object XInt     extends XPrimitive("Int")
  case object XLong    extends XPrimitive("Long")
  case object XFloat   extends XPrimitive("Float")
  case object XDouble  extends XPrimitive("Double")
  case object XBoolean extends XPrimitive("Boolean")

  case class XOptional(optionalType: XReference) extends XReference(XOptional.typename) with Parameterized {
    val typeParameters = optionalType :: Nil
  }
  
  case class XCollection(elementType: XReference, collection: XCollectionType) extends XReference(XCollection.typename) with Parameterized {
    val typeParameters = elementType :: Nil
  }

  case class XMap(keyType: XReference, valueType: XReference) extends XReference(XMap.typename) with Parameterized {
    val typeParameters = keyType :: valueType :: Nil
  }

  case class XTuple(types: List[XReference]) extends XReference(XTuple.typename) with Parameterized {
    def arity = types.length
    
    val typeParameters = types
  }

  sealed trait XField extends XSchema with Properties with Named with Parameterized {
    def fieldType: XReference
  }

  case class XRealField(fieldType: XReference, name: String, properties: Map[String, String], defValue: JValue, order: Order) extends XField with Default with Ordered {
    val typename = XRealField.typename
    
    val typeParameters = fieldType :: Nil
  }
  
  case class XViewField(fieldType: XReference, name: String, properties: Map[String, String]) extends XField {
    val typename = XViewField.typename
    
    val typeParameters = fieldType :: Nil
  }
  
  case class XProduct(namespace: Namespace, name: String, properties: Map[String, String], fields: List[XField]) extends XDefinition(XProduct.typename) {
    def elements = fields
    
    def viewFields: List[XViewField] = fields.filter(_.isInstanceOf[XViewField]).map(_.asInstanceOf[XViewField])
    
    def realFields: List[XRealField] = fields.filter(_.isInstanceOf[XRealField]).map(_.asInstanceOf[XRealField])
    
    def singleton = realFields.length == 0
  }
  
  case class XCoproduct(namespace: Namespace, name: String, properties: Map[String, String], types: List[XReference]) extends XDefinition(XCoproduct.typename) {
    def elements = types
  }
  
  case class XConstant(namespace: Namespace, name: String, properties: Map[String, String], constantType: XReference, defValue: JValue) extends XDefinition(XConstant.typename) with Default with Parameterized {
    val typeParameters = constantType :: Nil
  }

  object XReference {
    def apply(typename: String): XReference = typename match {
      case XString.typename     => XString
      case XInt.typename        => XInt
      case XLong.typename       => XLong
      case XFloat.typename      => XFloat
      case XDouble.typename     => XDouble
      case XBoolean.typename    => XBoolean
      
      case _ => new XReference(typename)
    }
  }
  
  object XRoot        extends TypeNamed("Root")
  object XView        extends TypeNamed("View")
  object XOptional    extends TypeNamed("Optional")
  object XCollection  extends TypeNamed("Collection")
  object XConstant    extends TypeNamed("Constant")
  object XMap         extends TypeNamed("Map")
  object XTuple       extends TypeNamed("Tuple")
  object XRealField   extends TypeNamed("Field")
  object XViewField   extends TypeNamed("ViewField")
  object XProduct     extends TypeNamed("Product")
  object XCoproduct   extends TypeNamed("Coproduct")
  
  trait XSchemaDefinitionWalker[T] {
    def begin(data: T, defn:  XRoot): T = data
    
    def begin(data: T, defn:  XDefinition): T = data
    def begin(data: T, field: XField): T = data
    def begin(data: T, opt:   XOptional): T = data
    def begin(data: T, col:   XCollection): T = data
    def begin(data: T, map:   XMap): T = data
    def begin(data: T, tuple: XTuple): T = data
    
    def walk(data: T, const:  XConstant): T = data
    def walk(data: T, prim:   XPrimitive): T = data
    def walk(data: T, ref:    XReference): T = data
    
    def end(data: T, defn:    XRoot): T = data    
    def end(data: T, defn:    XDefinition): T = data
    def end(data: T, field:   XField): T = data
    def end(data: T, opt:     XOptional): T = data
    def end(data: T, col:     XCollection): T = data
    def end(data: T, map:     XMap): T = data
    def end(data: T, tuple:   XTuple): T = data
  }
  
  def walk[T](s: XSchema, initial: T, walker: XSchemaDefinitionWalker[T]): T = {
    def walkContainer(initial: T, xs: Container): T = xs.elements.foldLeft[T](initial) { (cur, x) => walk(x, cur, walker) }
  
    s match {
      case x: XDefinition => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XField      => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XOptional   => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XCollection => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XMap        => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XTuple      => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XPrimitive  => walker.walk(initial, x)
      case x: XReference  => walker.walk(initial, x)
      
      case x: XRoot       => walker.end(walkContainer(walker.begin(initial, x), x), x)
    }
  }
  
  trait XSchemaDerived {
    def xschema: XSchema
  }
}

}