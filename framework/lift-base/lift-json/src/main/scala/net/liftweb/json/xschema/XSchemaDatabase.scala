package net.liftweb.json.xschema {

import XSchemaAST._

case class XSchemaValidation(warnings: List[String], errors: List[String])

/**
 * A database of schema definitions.
 */
trait XSchemaDatabase extends Iterable[XSchema] {
  def definitions: List[XSchemaDefinition]
  
  /**
   * Validates the definitions in the database, returning errors and warnings.
   */
  def validate: XSchemaValidation = {
    val errors = Nil
    val warnings = Nil
    
    XSchemaValidation(warnings, errors)
  }
  
  def allReferences = Set(all.map(_.typename): _*)
  
  /**
   * Finds all references to the specified schema.
   */
  def referencesTo(qualifiedName: String): List[XSchema] = all.filter(_.typename == qualifiedName)
  
  /**
   * Attempts to find the definition for the specified reference.
   */
  def definitionFor(ref: XSchemaReference): Option[XSchemaDefinition] = definitions.find(_.qualifiedName == ref.typename)

  /**
   * Resolves the type. Will return the passed in type except when it is a 
   * reference to a defined type, in which case it will return the definition 
   * for the type.
   */
  def resolve(ref: XSchemaReference): XSchema = ref match {
    case x: XFieldDefinition  => x
    case x: XOptional         => x
    case x: XCollection       => x
    case x: XMap              => x
    case x: XTuple            => x
    case x: XConstant         => x
    case x: XSchemaPrimitive  => x
    case x: XSchemaReference  => definitionFor(ref).get
  }
  
  def resolve(refs: Iterable[XSchemaReference]): List[XSchema] = refs.map(resolve(_)).toList
  
  /**
   * Returns all the containers of the specified definition.
   */
  def containersOf(defn: XSchemaDefinition) = all.flatMap { x => 
    x match {
      case x: Container => if (x.elements.exists(_.typename == defn.qualifiedName)) List(x) else Nil
      case _ => Nil
    }
  }
  
  /**
   * Returns all the coproducts that contain the specified definition.
   */
  def coproductContainersOf(defn: XProduct): List[XCoproduct] = containersOf(defn).flatMap { x =>
    x match {
      case x: XCoproduct => List[XCoproduct](x)
      case _ => Nil
    }
  }
  
  def isContainedInCoproduct(defn: XProduct) = coproductContainersOf(defn).length > 0
  
  /**
   * Retrieves all the product children of the specified coproduct.
   */
  def productChildrenOf(defn: XCoproduct): List[XProduct] = resolve(defn.types).map(_.asInstanceOf[XProduct])
  
  /**
   * Finds all fields that are common to all elements of the specified 
   * coproduct. Fields are common when they have the same name and type,
   * regardless of any other difference between them.
   */
  def commonFieldsOf(c: XCoproduct): List[(String, XSchemaReference)] = {
    val allFields: List[List[(String, XSchemaReference)]] = productChildrenOf(c).map(_.fields).map { fields => 
      // To simplify unification, map a product field to its coproduct containers (if there is one)
      fields.flatMap { field =>
        resolve(field.typep1) match {
          case p: XProduct if (isContainedInCoproduct(p)) => coproductContainersOf(p).map(c => (field.name, XSchemaReference(c.qualifiedName)))
          
          case _ => (field.name, field.typep1) :: Nil
        }
      }
    }
    
    allFields match {
      case Nil => Nil
      
      case head :: tail => tail.foldLeft(head) { (common, list) => common.intersect(list) }
    }
  }
  
  def elements = all.elements
  
  lazy val all = definitions.flatMap { x => build(x) }
  
  private def build(s: XSchema): List[XSchema] = s match {
    case x: Container => s :: x.elements.flatMap { y => build(y) }
    case _ => List(s)
  }
}

object XSchemaDatabase {
  import net.liftweb.json.JsonAST._
  import net.liftweb.json.JsonParser._
  
  def apply(d: List[XSchemaDefinition]): XSchemaDatabase = new XSchemaDatabase {
    val definitions = d
  }
  
  def apply(root: XSchemaRoot): XSchemaDatabase = apply(root.definitions)
  
  def apply(json: JValue): XSchemaDatabase = apply(XSchemaSerialization.extract(json).asInstanceOf[XSchemaRoot])
  
  def apply(str: String): XSchemaDatabase = apply(parse(str))
}

}