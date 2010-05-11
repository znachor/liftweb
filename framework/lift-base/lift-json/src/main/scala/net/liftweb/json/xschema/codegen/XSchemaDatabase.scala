package net.liftweb.json.xschema.codegen {

import net.liftweb.json.xschema._
import net.liftweb.json.xschema.XSchemaTree._

case class XSchemaValidation(warnings: List[String], errors: List[String])

/** A database of schema definitions.
 */
trait XSchemaDatabase extends Iterable[XSchema] {
  def definitions: List[XDefinition]
  
  /** Validates the definitions in the database, returning errors and warnings.
   */
  def validate: XSchemaValidation = {
    val errors = Nil
    val warnings = Nil
    
    XSchemaValidation(warnings, errors)
  }
  
  /** Attempts to find the definition for the specified reference.
   */
  def definitionFor(ref: XReference): Option[XDefinition] = ref match {
    case x: XDefinitionRef => definitions.find { y => y.name == x.name && y.namespace == x.namespace }
    
    case _ => None
  }
  
  lazy val constants = definitions.filter(constantsP).map(_.asInstanceOf[XConstant])
  
  lazy val products = definitions.filter(productsP).map(_.asInstanceOf[XProduct])
  
  lazy val coproducts = definitions.filter(coproductsP).map(_.asInstanceOf[XCoproduct])
  
  lazy val references = all.filter(referencesP).map(_.asInstanceOf[XReference])
  
  lazy val definitionRefs = all.filter(definitionRefsP).map(_.asInstanceOf[XDefinitionRef])
  
  lazy val primitiveRefs = all.filter(primitiveRefsP).map(_.asInstanceOf[XPrimitiveRef])
  
  def definitionsIn(namespace: String): List[XDefinition] = definitions.filter(_.namespace == namespace)
  
  def constantsIn(namespace: String) = constants.filter(_.namespace == namespace)
  
  def productsIn(namespace: String) = products.filter(_.namespace == namespace)
  
  def coproductsIn(namespace: String) = coproducts.filter(_.namespace == namespace)
  
  def definitionRefsIn(namespace: String) = definitionRefs.filter(_.namespace == namespace)
  
  /** Retrieves all the namespaces.
   */
  def namespaces = definitions.map(_.namespace).removeDuplicates

  /** Resolves the type. Will return the passed in type except when it is a 
   * reference to a defined type, in which case it will return the definition 
   * for the type.
   */
  def resolve(ref: XReference): XSchema = definitionFor(ref).getOrElse(ref)
  
  def resolve(refs: Iterable[XReference]): List[XSchema] = refs.map(resolve(_)).toList
  
  def elementsOf(s: XSchema): List[XReference] = s match {
    case x: XProduct    => x.terms.map(_.fieldType)
    case x: XCoproduct  => x.terms
    case x: XField      => x.fieldType :: Nil
    case x: XOptional   => x.optionalType :: Nil
    case x: XCollection => x.elementType :: Nil
    case x: XMap        => x.keyType :: x.valueType :: Nil
    case x: XTuple      => x.types
    case _ => Nil
  }
  
  /** Returns all the resolved containers of the specified definition.
   */
  def containersOf(defn: XDefinition) = definitions.filter { x =>
    elementsOf(x).contains(defn.referenceTo)
  }
  
  /** Returns all the coproducts that contain the specified definition.
   */
  def coproductContainersOf(defn: XDefinition): List[XCoproduct] = containersOf(defn).flatMap { 
    case x: XCoproduct => List[XCoproduct](x)
    case _ => Nil
  }
  
  def isContainedInCoproduct(defn: XProduct) = coproductContainersOf(defn).length > 0
  
  /** Retrieves all the product children of the specified coproduct.
   */
  def namespacesOf(defn: XCoproduct): List[String] = resolve(defn.terms).map { x =>
    x match {
      case x: XProduct => x.namespace
      case x: XCoproduct => x.namespace
      
      case x: XDefinitionRef => x.namespace
      
      case _ => error("cannot find namespace")
    }
  }
  
  /** Finds all fields that are common to all elements of the specified 
   * coproduct. Fields are common when they have the same name and type,
   * regardless of any other difference between them.
   */
  def commonFieldsOf(c: XCoproduct): List[(String, XReference)] = {
    val allFields: List[List[(String, XReference)]] = c.terms.map(resolve(_)).map { resolved => 
        resolved match {
          case p: XProduct => p.terms.flatMap { field =>
            resolve(field.fieldType) match {
              case p: XProduct if (isContainedInCoproduct(p)) => coproductContainersOf(p).map(c => (field.name, c.referenceTo))

              case _ => (field.name, field.fieldType) :: Nil
            }
          }
        
          case c: XCoproduct => commonFieldsOf(c)
          
          case _ => Nil
      }
    }
    
    allFields match {
      case Nil => Nil
      
      case head :: tail => tail.foldLeft(head) { (common, list) => common.intersect(list) }
    }
  }
  
  def elements = all.elements
  
  lazy val all = definitions.flatMap { x => build(x) }
  
  private def build(s: XSchema): List[XSchema] = s :: elementsOf(s).flatMap(build _)
  
  private def constantsP = (x: XSchema) => x.isInstanceOf[XConstant]
  
  private def productsP = (x: XSchema) => x.isInstanceOf[XProduct]
  
  private def coproductsP = (x: XSchema) => x.isInstanceOf[XCoproduct]
  
  private def referencesP = (x: XSchema) => x.isInstanceOf[XReference]
  
  private def definitionRefsP = (x: XSchema) => x.isInstanceOf[XDefinitionRef]
  
  private def primitiveRefsP = (x: XSchema) => x.isInstanceOf[XPrimitiveRef]
}

object XSchemaDatabase {
  import net.liftweb.json.JsonAST._
  import net.liftweb.json.JsonParser._
  import net.liftweb.json.xschema.Serialization._
  
  def apply(d: List[XDefinition]): XSchemaDatabase = new XSchemaDatabase {
    val definitions = d
  }
  
  def apply(root: XRoot): XSchemaDatabase = apply(root.definitions)
  
  //def apply(json: JValue): XSchemaDatabase = apply(json.deserialize[XSchemaRoot].asInstanceOf)
  
  def apply(str: String): XSchemaDatabase = apply(parse(str))
}

}