package ${packageName}.snippet

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.java.util.Date
import Helpers._
import ${packageName}.lib._

class HelloWorld {
  import DependencyFactory._ // Import the Dependency Injector

  val date = inject[Date] // inject the date
  // lazy val date = DependencyFactory.time.make // create the date via factory

  def howdy(in: NodeSeq): NodeSeq =
    Helpers.bind("b", in, "time" -> date.map(d => Text(d.toString)))
}


