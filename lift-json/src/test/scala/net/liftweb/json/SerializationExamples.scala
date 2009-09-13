package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class SerializationExamplesTest extends Runner(SerializationExamples) with JUnit
object SerializationExamples extends Specification {
  import Serialization._

  val project = Project("test", Language("Scala", 2.75), List(
    Team("QA", List(Employee("John Doe", 5), Employee("Mike", 3))),
    Team("Impl", List(Employee("Mark", 4), Employee("Mary", 5), Employee("Nick Noob", 1)))))

  "Project serialization example" in {
    val ser = save(project)
    load[Project](ser) mustEqual project
  }
  
  case class Project(name: String, lang: Language, teams: List[Team])
  case class Language(name: String, version: Double)
  case class Team(role: String, members: List[Employee])
  case class Employee(name: String, experience: Int)
}
