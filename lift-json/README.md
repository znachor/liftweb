Parsing and formatting utilities for JSON.

DSL rules
---------

* Primitive types map to JSON primitives.
* Any seq produces JSON array.

      scala> val json = List(1, 2, 3)

      scala> compact(JsonAST.render(json))

      res0: String = [1,2,3]

* Tuple2[String, A] produces field.

      scala> val json = ("name" -> "joe")

      scala> compact(JsonAST.render(json))

      res1: String = {"name":"joe"}

* ~ operator produces object by combining fields.

      scala> val json = ("name" -> "joe") ~ ("age" -> 35)

      scala> compact(JsonAST.render(json))

      res2: String = {"name":"joe","age":35}

* Any value can be optional. Field and value is completely removed when it doesn't have a value.

      scala> val json = ("name" -> "joe") ~ ("age" -> Some(35))

      scala> compact(JsonAST.render(json))

      res3: String = {"name":"joe","age":35}

      scala> val json = ("name" -> "joe") ~ ("age" -> (None: Option[Int]))

      scala> compact(JsonAST.render(json))

      res4: String = {"name":"joe"}

Example
-------

    object JsonExample extends Application {
      import net.liftweb.json.JsonAST
      import net.liftweb.json.JsonDSL._

      case class Winner(id: Long, numbers: List[Int])
      case class Lotto(id: Long, winningNumbers: List[Int], winners: List[Winner], drawDate: Option[java.util.Date])

      val winners = List(Winner(23, List(2, 45, 34, 23, 3, 5)), Winner(54, List(52, 3, 12, 11, 18, 22)))
      val lotto = Lotto(5, List(2, 45, 34, 23, 7, 5, 3), winners, None)

      val json = 
        ("lotto" ->
          ("lotto-id" -> lotto.id) ~
          ("winning-numbers" -> lotto.winningNumbers) ~
          ("draw-date" -> lotto.drawDate.map(_.toString)) ~
          ("winners" ->
            lotto.winners.map { w =>
              (("winner-id" -> w.id) ~
               ("numbers" -> w.numbers))}))

      println(compact(JsonAST.render(json)))
    }

    scala> JsonExample
    {"lotto":{"lotto-id":5,"winning-numbers":[2,45,34,23,7,5,3],"winners":
    [{"winner-id":23,"numbers":[2,45,34,23,3,5]},{"winner-id":54,"numbers":[52,3,12,11,18,22]}]}}

Example produces following pretty printed JSON. Notice that draw-date field is not rendered since its value is None:

    scala> pretty(render(JsonExample.json))

    {
      "lotto":{
        "lotto-id":5,
        "winning-numbers":[2,45,34,23,7,5,3],
        "winners":[{
          "winner-id":23,
          "numbers":[2,45,34,23,3,5]
        },{
          "winner-id":54,
          "numbers":[52,3,12,11,18,22]
        }]
      }
    }

Parsing
-------

Any valid json can be parsed into internal AST format.

    scala> import net.liftweb.json.JsonParser._
    scala> parse(""" { "numbers" : [1, 2, 3, 4] } """)
    res0: net.liftweb.json.JsonAST.JValue = 
          JObject(List(JField(numbers,JArray(List(JInt(1), JInt(2), JInt(3), JInt(4))))))

Queries
-------

Json AST can be queried using XPath like functions. Following REPL session shows the usage of 
'\\', '\\\\', 'find', 'filter', 'map' and 'values' functions. 

    The example json is:

    { 
      "person": {
        "name": "Joe",
        "age": 35,
        "spouse": {
          "person": {
            "name": "Marilyn"
            "age": 33
          }
        }
      }
    }

    Translated to DSL syntax:

    scala> import net.liftweb.json.JsonAST._
    scala> import net.liftweb.json.JsonDSL._

    scala> val json = 
      ("person" ->
        ("name" -> "Joe") ~
        ("age" -> 35) ~
        ("spouse" -> 
          ("person" -> 
            ("name" -> "Marilyn") ~
            ("age" -> 33)
          )
        )
      )

    scala> json \\ "spouse"
    res0: net.liftweb.json.JsonAST.JValue = JObject(List(JField(spouse,JObject(List(
          JField(person,JObject(List(JField(name,JString(Marilyn)), JField(age,JInt(33))))))))))

    scala> compact(render(res0))
    res1: String = {"spouse":{"person":{"name":"Marilyn","age":33}}}

    scala> compact(render(json \\ "name"))
    res2: String = {"name":"Joe","name":"Marilyn"}

    scala> compact(render(json \ "person" \ "name"))
    res3: String = "name":"Joe"

    scala> compact(render(json \ "person" \ "spouse" \ "person" \ "name"))
    res4: String = "name":"Marilyn"

    scala> json find {
             case JField("name", _) => true
             case _ => false
           }
    res5: Option[net.liftweb.json.JsonAST.JValue] = Some(JField(name,JString(Joe)))

    scala> json filter {
             case JField("name", _) => true
             case _ => false
           }
    res6: List[net.liftweb.json.JsonAST.JValue] = List(JField(name,JString(Joe)), JField(name,JString(Marilyn)))

    scala> json.values
    res7: net.liftweb.json.JsonAST.JValue#Values = Map(person -> Map(name -> Joe, age -> 35, spouse -> Map(person -> Map(name -> Marilyn, age -> 33))))

Indexed path expressions work too, and values can be extracted using for-comprehensions.

    scala> val json = parse("""
             { "name": "joe",
               "children": [
                 {
                   "name": "Mary",
                   "age": 5
                 },
                 {
                   "name": "Mazy",
                   "age": 3
                 }
               ]
             }
           """)

    scala> (json \ "children")(0)
    res0: net.liftweb.json.JsonAST.JValue = JObject(List(JField(name,JString(Mary)), JField(age,JInt(5))))

    scala> (json \ "children")(1) \ "name"
    res1: net.liftweb.json.JsonAST.JValue = JField(name,JString(Mazy))

    scala> for { JField("age", y) <- json } yield y
    res2: List[net.liftweb.json.JsonAST.JValue] = List(JInt(5), JInt(3))

    scala> for { JField("age", JInt(y)) <- json } yield y
    res3: List[BigInt] = List(5, 3)

Extracting values
-----------------

Case classes can be used to extract values from parsed JSON. Non-existing values
can be extracted into scala.Option and strings can be automatically converted into
java.util.Dates.
Please see more examples in src/test/scala/net/liftweb/json/ExtractionExamples.scala

    scala> implicit val formats = net.liftweb.json.DefaultFormats // Brings in default date formats etc.
    scala> case class Child(name: String, age: Int, birthdate: Option[java.util.Date])
    scala> case class Address(street: String, city: String)
    scala> case class Person(name: String, address: Address, children: List[Child])
    scala> import net.liftweb.json.JsonParser._
    scala> val json = parse("""
             { "name": "joe",
               "address": {
                 "street": "Bulevard",
                 "city": "Helsinki"
               },
               "children": [
                 {
                   "name": "Mary",
                   "age": 5
		   "birthdate": "2004-09-04T18:06:22Z"
                 },
                 {
                   "name": "Mazy",
                   "age": 3
                 }
               ]
             }
           """)

    scala> json.extract[Person] 
    res0: Person = Person(joe,Address(Bulevard,Helsinki),List(Child(Mary,5,Some(Sat Sep 04 18:06:22 EEST 2004)), Child(Mazy,3,None)))


Kudos
-----

* The original idea for DSL syntax was taken from Lift mailing list ([by Marius](http://markmail.org/message/lniven2hn22vhupu)).

* The idea for AST and rendering was taken from [Real World Haskell book](http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html).
