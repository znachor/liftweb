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

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

class JsDependenciesSpecTest extends Runner(JsDependenciesSpec) with JUnit with Console
object JsDependenciesSpec extends Specification {
  val noDep = new JsScript(Nil)
  val noDepScripts = noDep :: Nil
  val oneDep = new JsScript(Nil) {
    override def dependencies = noDepScripts
  }
  val oneDepScripts = oneDep :: Nil
  val noAndOneDepScripts = noDep :: oneDep :: Nil
  val oneAndNoDepScripts = oneDep :: noDep :: Nil

  "A List only containing a JsScript without depedencies" should {
    "satisfy all dependencies" in {
      JsDependencies.satisfied_?(noDepScripts) must_== true
    }
  }

  "A List only containing a JsScript without depedencies" should {
    "resolve to itself" in {
      JsDependencies.resolve(noDepScripts) must_== noDepScripts
    }
  }

  "A List only containing a JsScript with a depedency" should {
    "not satisfy all dependencies" in {
      JsDependencies.satisfied_?(oneDepScripts) must_== false
    }
  }

  "A List only containing a JsScript with a depedency" should {
    "resolve to a List containing its dependency and then itself" in {
      JsDependencies.resolve(oneDepScripts) must_== noAndOneDepScripts
    }
  }

  """A List containing a JsScript without dependencies and then a JsScript whose
     only dependency is the first JsScript""" should {
    "satisfy all dependencies" in {
      JsDependencies.satisfied_?(noAndOneDepScripts) must_== true
    }
  }

  """A List containing a JsScript without dependencies and then a JsScript whose
     only dependency is the first JsScript""" should {
    "resolve to itself" in {
      JsDependencies.resolve(noAndOneDepScripts) must_== noAndOneDepScripts
    }
  }

  """A List containing a JsScript followed by a JsScript without dependencies
     upon which the first depends""" should {
    "not satisfy all dependencies" in {
      JsDependencies.satisfied_?(oneAndNoDepScripts) must_== false
    }
  }

  """A List containing a JsScript followed by a JsScript without dependencies
     upon which the first depends""" should {
    """resolve to a List consisting of the JsScript without dependencies and
      then the JsScript dependent upon it""" in {
      JsDependencies.resolve(oneAndNoDepScripts) must_== noAndOneDepScripts
    }
  }
}

}
}
}