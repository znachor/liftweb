package net.liftweb.json.xschema {

import XSchemaAST._

trait XSchemaDatabase extends Iterable[XSchema] {
  def schemas: List[XSchema]
  
  def references(schema: XSchema with Named)
  
  lazy val all = schemas.flatMap { x => build(x) }
  
  private def build(s: XSchema): List[XSchema] = s match {
    case x: Container => s :: x.elements.flatMap { y => build(y) }
    case _ => List(s)
  }
}

}