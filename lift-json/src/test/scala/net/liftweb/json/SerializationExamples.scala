package net.liftweb.json

import java.util.Date
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class SerializationExamplesTest extends Runner(SerializationExamples) with JUnit
object SerializationExamples extends Specification {
  import Serialization._

  val project = Project("test", new Date, Some(Language("Scala", 2.75)), List(
    Team("QA", List(Employee("John Doe", 5), Employee("Mike", 3))),
    Team("Impl", List(Employee("Mark", 4), Employee("Mary", 5), Employee("Nick Noob", 1)))))

  "Project serialization example" in {
    val ser = save(project)
    load[Project](ser) mustEqual project
  }
  
  case class Project(name: String, startDate: Date, lang: Option[Language], teams: List[Team])
  case class Language(name: String, version: Double)
  case class Team(role: String, members: List[Employee])
  case class Employee(name: String, experience: Int)

  "Lotto serialization example" in {
    import LottoExample.{Lotto, lotto}

    val ser = save(lotto)
    load[Lotto](ser) mustEqual lotto
  }

  "Primitive serialization example" in {
    val primitives = Primitives(124, 123L, 126.5, 127.5.floatValue, "128", 125, 129.byteValue, true)
    val ser = save(primitives)
    load[Primitives](ser) mustEqual primitives
  }

}
