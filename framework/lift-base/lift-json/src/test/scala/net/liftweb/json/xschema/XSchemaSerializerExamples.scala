package net.liftweb.json.xschema {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class XSchemaSerializerExamplesTest extends Runner(XSchemaSerializerExamples) with JUnit

object XSchemaSerializerExamples extends Specification {
  import JsonAST._
  import JsonParser._
  import XSchemaAST._
  import XSchemaSerialization._
  import TestSchemas._
  
  "Extraction and decomposition are symmetric" in {
    println(Printer.pretty(render(decompose(AstNumericExprSchema))))
    
    extract(decompose(AstNumericExprSchema)) mustEqual AstNumericExprSchema
  }
}

object TestSchemas {
  import JsonAST._
  import JsonParser._
  import XSchemaAST._
  import XSchemaSerialization._
  
  val numberExpr = parse("""{ "Number": { "value": 0.0 } }""")
  val number     = parse("""{ "value": 0.0 }""")

  val AstNumericExprSchema = XSchemaRoot(
    1,
    List(
      XCoproduct(
        Namespace("ast.numeric"),
        "Expr",
        Map(),
        List(
          XSchemaReference("ast.numeric.NumericSum"),
          XSchemaReference("ast.numeric.Sum"),
          XSchemaReference("ast.numeric.Product"),
          XSchemaReference("ast.numeric.Number")
        )
      ),
      XCoproduct(
        Namespace("ast.numeric"),
        "MixedSum",
        Map(),
        List(
          XSchemaReference("ast.numeric.NumericSum"),
          XSchemaReference("ast.numeric.Sum")
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "NumericSum",
        Map(),
        List(
          XFieldDefinition(XSchemaReference("ast.numeric.Number"), "term1", Map(), numberExpr, Descending),
          XFieldDefinition(XSchemaReference("ast.numeric.Number"), "term2", Map(), numberExpr, Descending)
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "Sum",
        Map(),
        List(
          XFieldDefinition(XSchemaReference("ast.numeric.Expr"), "term1", Map(), numberExpr, Descending),
          XFieldDefinition(XSchemaReference("ast.numeric.Expr"), "term2", Map(), numberExpr, Descending)
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "Product",
        Map(),
        List(
          XFieldDefinition(XSchemaReference("ast.numeric.Expr"), "term1", Map(), numberExpr, Descending),
          XFieldDefinition(XSchemaReference("ast.numeric.Expr"), "term2", Map(), numberExpr, Descending)
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "Number",
        Map(),
        List(
          XFieldDefinition(XDouble, "value", Map(), number, Descending)
        )
      )
    ),
    Map()
  )

  val DataSocialGenderSchema = XSchemaRoot(
    1,
    List(
      XCoproduct(
        Namespace("data.social"),
        "Gender",
        Map(),
        List(
          XSchemaReference("data.social.Male"),
          XSchemaReference("data.social.Female")
        )
      ),
      XProduct(
        Namespace("data.social"),
        "Male",
        Map(),
        List(
          XFieldDefinition(XString, "text", Map(), JString("male"), Descending)
        )
      ),
      XProduct(
        Namespace("data.social"),
        "Female",
        Map(),
        List(
          XFieldDefinition(XString, "text", Map(), JString("female"), Descending)
        )
      )
    ),
    Map()
  )
}

}
