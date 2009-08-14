package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class LottoExampleTest extends Runner(LottoExample) with JUnit
object LottoExample extends Specification {
  import JsonAST._
  import JsonDSL._

  case class Winner(id: Long, numbers: List[Int])
  case class Lotto(id: Long, winningNumbers: List[Int], winners: List[Winner], drawDate: Option[java.util.Date])

  val winners = List(Winner(23, List(2, 45, 34, 23, 3, 5)), Winner(54, List(52, 3, 12, 11, 18, 22)))
  val lotto = Lotto(5, List(2, 45, 34, 23, 7, 5, 3), winners, None)

  // FIXME add annnotation to configure path
  val json = 
    ("lotto" ->
      ("id" -> lotto.id) ~
      ("winningNumbers" -> lotto.winningNumbers) ~
      ("drawDate" -> lotto.drawDate.map(_.toString)) ~
      ("winners" ->
        lotto.winners.map { w =>
          (("id" -> w.id) ~
           ("numbers" -> w.numbers))}))

  compact(render(json)) mustEqual """{"lotto":{"id":5,"winningNumbers":[2,45,34,23,7,5,3],"winners":[{"id":23,"numbers":[2,45,34,23,3,5]},{"id":54,"numbers":[52,3,12,11,18,22]}]}}"""

//  ((json \ "lotto" \ "winners")(0)).extract[Winner] mustEqual Winner(23, List(2, 45, 34, 23, 3, 5))
}
