/*
 * Copyright 2010-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package http {
package js {

import _root_.net.liftweb.common.{Box, Full, Empty}
import _root_.scala.xml.{Elem, NodeSeq, Text}

/*
 * JsScript represents a Javascript file throughout a Lift application. Its
 * dependencies are other Javascript files it requires to function properly and
 * so must be included in an HTML page before the file.
 */
class JsScript(path: List[String]) {
  def dependencies: List[JsScript] = Nil
  def baseUrl = ""
  def allowResource: PartialFunction[List[String], Boolean] = {
    case path => true
  }
  def toHTML =
    <script type="text/javascript" src={ path.mkString(baseUrl, "/", "") }>
    </script>
}

/*
 * JsDependencies is a trait that can be mixed into CometActors to ensure that
 * all necessary Javascript files are included upon the initial render of the
 * HTML page.
 */
trait JsDependencies extends CometActor {
  def scripts: List[JsScript];
  abstract override lazy val fixedRender: Box[NodeSeq] =
    // FIXME: Better done as a for comprehension?
    Some(NodeSeq.fromSeq(
      JsDependencies.resolve(scripts).map(_.toHTML) ++
      super.fixedRender.getOrElse(Text(""))
    ))
}

/*
 * JsDependencies is a helper object used to resolve all the dependences for a
 * List of JsScripts.
 */
object JsDependencies {
  /*
   * Recursively resolves all depedencies for a List of JsScripts.
   */
  def resolve(dependencies: List[JsScript]): List[JsScript] = {
    satisfied_?(dependencies) match {
      case false => resolve(
        resolve(dependencies.flatMap(
          script => script.dependencies ::: script :: Nil
        /*
         * NOTE: In Scala 2.8 removeDuplicates takes the first, not last,
         * occurance of each duplicate so the reverse calls need to be removed.
         */
        ).reverse.removeDuplicates.reverse
      ))
      case true => dependencies
    }
  }

  /*
   * Returns true or false depending on whether each JsScript's set of 
   * dependencies is satisfied. NOTE: List order matters.
   */
  def satisfied_?(dependencies: List[JsScript]): Boolean = {
    dependencies.map(jsScript =>
      /*
       * test whether all of the JsScript's dependencies are found  in the
       * dependencies list prior to the JsScript in question.
       */
      jsScript.dependencies.intersect(dependencies.takeWhile(
        item => item != jsScript)
      ) == jsScript.dependencies
    // combine whether all into one value for the entire List
    ).reduceLeft((dependencyA, dependencyB) =>
      dependencyA == dependencyB == true
    )
  }
}

}
}
}