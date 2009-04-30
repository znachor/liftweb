/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.example.comet

import scala.actors.Actor
import Actor._
import scala.xml.{NodeSeq, Text}

import net.liftweb._
import http._
import js._
import JsCmds._

import util._
import Helpers._

// a singleton that builds a "thing"
object ThingBuilder extends Actor {
  def boot() {
    LiftRules.dispatch.append {
      case Req("getit":: Nil, _, GetRequest) =>
        () => Full(XmlResponse(<info>Here's some info</info>))
    }
  }

  def act = loop {
    react {
      case a: Actor =>
        this ! (a, 1)

      case (a: Actor, 10) =>
        a ! <b>Your thing is <a href="/getit">done</a></b>
        a ! RedirectTo("/getit")

      case (a: Actor, i: Int) =>
        a ! ("Working on making your thing... "+i)
        ActorPing.schedule(this, (a, i + 1), 2 seconds)

      case _ =>
    }
  }

  this.start
}

class LongTime extends CometActor {
  private var url: Box[NodeSeq] = Empty
  private var msg = "Working"

  override def lifespan: Box[TimeSpan] = Full(2 minutes)

  override def highPriority = {
    case n: NodeSeq => url = n
      reRender(false)

    case js: JsCmd =>
      partialUpdate(js)

    case s: String =>
      msg = s
      reRender(false)
  }

  override def localSetup() {
    ThingBuilder ! this
    super.localSetup()
  }

  def render = {
    val ns = url openOr Text(msg)
    ns
  }
}
