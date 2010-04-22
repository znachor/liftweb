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

  /** All schemas are one of: root, reference, type definition, field definition, or constant */
  sealed trait XSchema extends Typed

  sealed case class XRoot(version: Int, definitions: List[XDefinition], properties: Map[String, String]) extends TypeNamed(XRoot.typename) with XSchema with Properties with Versioned with Container {
    def elements = definitions
  }
  
  sealed class XReference protected (val typename: String) extends XSchema {
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
  
  sealed case class XConstant(constantType: XReference, defValue: JValue) extends XReference(XConstant.typename) with Default with Parameterized {
    val typeParameters = constantType :: Nil
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

  case class XMap(valueType: XReference) extends XReference(XMap.typename) with Parameterized {
    val typeParameters = valueType :: Nil
  }

  case class XTuple(types: List[XReference]) extends XReference(XTuple.typename) with Parameterized {
    def arity = types.length
    
    val typeParameters = types
  }

  case class XFieldDefinition(fieldType: XReference, name: String, properties: Map[String, String], defValue: JValue, order: Order) extends XSchema with Properties with Named with Default with Parameterized with Ordered {
    val typename = XFieldDefinition.typename
    
    val typeParameters = fieldType :: Nil
  }
  
  case class XProduct(namespace: Namespace, name: String, properties: Map[String, String], fields: List[XFieldDefinition]) extends XDefinition(XProduct.typename) {
    def elements = fields
  }
  
  case class XCoproduct(namespace: Namespace, name: String, properties: Map[String, String], types: List[XReference]) extends XDefinition(XCoproduct.typename) {
    def elements = types
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
  
  object XRoot            extends TypeNamed("Root")
  object XOptional        extends TypeNamed("Optional")
  object XCollection      extends TypeNamed("Collection")
  object XConstant        extends TypeNamed("Constant")
  object XMap             extends TypeNamed("Map")
  object XTuple           extends TypeNamed("Tuple")
  object XFieldDefinition extends TypeNamed("Field")
  object XProduct         extends TypeNamed("Product")
  object XCoproduct       extends TypeNamed("Coproduct")
  
  trait XSchemaDefinitionWalker[T] {
    def begin(data: T, defn:  XRoot): T = data
    
    def begin(data: T, defn:  XDefinition): T = data
    def begin(data: T, field: XFieldDefinition): T = data
    def begin(data: T, opt:   XOptional): T = data
    def begin(data: T, col:   XCollection): T = data
    def begin(data: T, map:   XMap): T = data
    def begin(data: T, tuple: XTuple): T = data
    
    def walk(data: T, const:  XConstant): T = data
    def walk(data: T, prim:   XPrimitive): T = data
    def walk(data: T, ref:    XReference): T = data
    
    def end(data: T, defn:    XRoot): T = data    
    def end(data: T, defn:    XDefinition): T = data
    def end(data: T, field:   XFieldDefinition): T = data
    def end(data: T, opt:     XOptional): T = data
    def end(data: T, col:     XCollection): T = data
    def end(data: T, map:     XMap): T = data
    def end(data: T, tuple:   XTuple): T = data
  }
  
  def walk[T](s: XSchema, initial: T, walker: XSchemaDefinitionWalker[T]): T = {
    def walkContainer(initial: T, xs: Container): T = xs.elements.foldLeft[T](initial) { (cur, x) => walk(x, cur, walker) }
  
    s match {
      case x: XDefinition       => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XFieldDefinition  => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XOptional         => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XCollection       => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XMap              => walker.end(walkContainer(walker.begin(initial, x), x), x)
      case x: XTuple            => walker.end(walkContainer(walker.begin(initial, x), x), x)
      
      case x: XConstant     => walker.walk(initial, x)
      case x: XPrimitive    => walker.walk(initial, x)
      case x: XReference    => walker.walk(initial, x)
      
      case x: XRoot         => walker.end(walkContainer(walker.begin(initial, x), x), x)
    }
  }
}

}