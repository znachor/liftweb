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

  val AstNumericExprSchema = XRoot(
    1,
    List(
      XCoproduct(
        Namespace("ast.numeric"),
        "Expr",
        Map(),
        List(
          XReference("ast.numeric.NumericSum"),
          XReference("ast.numeric.Sum"),
          XReference("ast.numeric.Product"),
          XReference("ast.numeric.Number")
        )
      ),
      XCoproduct(
        Namespace("ast.numeric"),
        "MixedSum",
        Map(),
        List(
          XReference("ast.numeric.NumericSum"),
          XReference("ast.numeric.Sum")
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "NumericSum",
        Map(),
        List(
          XFieldDefinition(XReference("ast.numeric.Number"), "term1", Map(), numberExpr, Descending),
          XFieldDefinition(XReference("ast.numeric.Number"), "term2", Map(), numberExpr, Descending)
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "Sum",
        Map(),
        List(
          XFieldDefinition(XReference("ast.numeric.Expr"), "term1", Map(), numberExpr, Descending),
          XFieldDefinition(XReference("ast.numeric.Expr"), "term2", Map(), numberExpr, Descending)
        )
      ),
      XProduct(
        Namespace("ast.numeric"),
        "Product",
        Map(),
        List(
          XFieldDefinition(XReference("ast.numeric.Expr"), "term1", Map(), numberExpr, Descending),
          XFieldDefinition(XReference("ast.numeric.Expr"), "term2", Map(), numberExpr, Descending)
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

  val DataSocialGenderSchema = XRoot(
    1,
    List(
      XCoproduct(
        Namespace("data.social"),
        "Gender",
        Map(),
        List(
          XReference("data.social.Male"),
          XReference("data.social.Female")
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
