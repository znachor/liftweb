package ${packageName}.snippet

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb._
import http._
import net.liftweb.util._
import org.specs.matcher._
import Helpers._
import lib._


class HelloWorldTestSpecsAsTest extends JUnit3(HelloWorldTestSpecs)
object HelloWorldTestSpecsRunner extends ConsoleRunner(HelloWorldTestSpecs)

object HelloWorldTestSpecs extends Specification {
  val session = new LiftSession("", randomString(20), Empty)

  "HelloWorld Snippet" should {
    "Put the time in the node" in {
      S.initIfUninitted(session) {
        val stableTime = now
        DependencyFactory.time.doWith(stableTime) {
          val hello = new HelloWorld
          Thread.sleep(1000) // make sure the time changes

          val str = hello.howdy(<span>Hello at <b:time/></span>).toString

          str.indexOf(stableTime.toString) must be >= 0
          str.indexOf("Hello at") must be >= 0
        }
      }
    }
  }
}

