package net.liftweb.wizard

import _root_.org.scalacheck._
import _root_.org.scalacheck.Prop.forAll
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.ScalaCheck

class WizardTest extends Runner(WizardSpec) with JUnit
object WizardSpec extends Specification {
  val MyWizard = new Wizard {
    val firstScreen = new Screen {
      val firstField = new Field with IntField {
        def title = "age"
      }
    }
  }


  "A Wizard can be defined" in {
    MyWizard.firstScreen.name must_== "Screen 1"
  }

  "A field must have a correct Manifest" in {
    MyWizard.firstScreen.firstField.manifest.erasure.getName must_== classOf[Int].getName
  }
}
