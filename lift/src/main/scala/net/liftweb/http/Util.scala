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

package net.liftweb.http

import util._
import Helpers._

import _root_.scala.actors._
import Actor._


/**
 * An actor that monitors other actors that are linked with it. If a watched
 * actor terminates,this actor captures the Exit messag, executes failureFuncs
 * and resurects the actor.
 */
object ActorWatcher extends Actor {
  def act = loop {
    react {
      case Exit(actor: Actor, why: Throwable) =>
        failureFuncs.foreach(f => tryo(f(actor, why)))

      case _ =>
    }
  }

  private def startAgain(a: Actor, ignore: Throwable) {
    a.start
    a ! RelinkToActorWatcher
  }

  private def logActorFailure(actor: Actor, why: Throwable) {
    Log.error("The ActorWatcher restarted "+actor+" because "+why, why)
  }

  /**
   * If there's something to do in addition to starting the actor up, pre-pend the
   * actor to this List
   */
  var failureFuncs: List[(Actor, Throwable) => Unit] = logActorFailure _ ::
  startAgain _ :: Nil

  this.trapExit = true
  this.start
}

case object RelinkToActorWatcher


object ActorSchedulerFixer {
  var performFix = true
  import java.util.concurrent.{Executors, Executor}

  private var fixDone = false

  var exeuctorCreator: () => Executor = Executors.newCachedThreadPool _

  var doLogging: Throwable => Unit =
  e => Log.error("Actor scheduler", e)

  var runnableCreator: (() => Unit) => Runnable =
  toRun => new Runnable {
    def run() {
      try {
        toRun.apply()
      } catch {
        case e => doLogging(e)
      }
    }
  }

  def doActorSchedulerFix(): Unit = synchronized {

    if (performFix && !fixDone && !Props.inGAE) {
      Scheduler.impl match {
        case fj: FJTaskScheduler2 =>
          fj.snapshot()
          fj.shutdown()
        case _ =>
      }

      Scheduler.impl = {
        val es = exeuctorCreator()

        new IScheduler {

          /** Submits a closure for execution.
           *
           *  @param  fun  the closure to be executed
           */
          def execute(fun: => Unit): Unit = {
          try {

          es.execute(runnableCreator(() => fun))
          } catch {
            case e => e.printStackTrace
          }
          }

          /** Submits a <code>Runnable</code> for execution.
           *
           *  @param  task  the task to be executed
           */
          def execute(task: Runnable): Unit = {
            try {

          es.execute(runnableCreator(() => task.run))
            } catch {
              case e => e.printStackTrace
            }
          }

          /** Notifies the scheduler about activity of the
           *  executing actor.
           *
           *  @param  a  the active actor
           */
          def tick(a: Actor): Unit = {}

          /** Shuts down the scheduler.
           */
          def shutdown(): Unit = {}

          def onLockup(handler: () => Unit): Unit = {}
          def onLockup(millis: Int)(handler: () => Unit): Unit = {}
          def printActorDump: Unit = {}
        }
      }
    }
    fixDone = true
  }
}

object PointlessActorToWorkAroundBug extends Actor {
  import scala.collection.mutable.HashSet
  import java.lang.ref.Reference
  import java.lang.reflect.Field

  private def findField(in: Class[_], name: String): Box[Field] =
  in match {
    case null => Empty
    case in => tryo(in.getDeclaredField(name)) or findField(in.getSuperclass, name)
  }

  def act = loop {
    react {
      case "ActorBug" =>
        try {
          import scala.collection.mutable.HashSet
          import java.lang.ref.Reference

          val agc = ActorGC
          agc.synchronized {
            val rsf = agc.getClass.getDeclaredField("refSet")
            rsf.setAccessible(true)
            rsf.get(agc) match {
              case h: HashSet[Reference[Object]] =>
                Log.trace("[MEMDEBUG] got the actor refSet... length: "+h.size)

                val nullRefs = h.elements.filter(f => f.get eq null).toList

                nullRefs.foreach(r => h -= r)

                val nonNull = h.elements.filter(f => f.get ne null).toList

                Log.trace("[MEMDEBUG] got the actor refSet... non null elems: "+
                          nonNull.size)

                nonNull.foreach{r =>
                  for
                  {
                    a <- findField(r.get.getClass, "exiting")
                  } {
                    a.setAccessible(true)
                    if (a.getBoolean(r.get)) {
                      h -= r
                      r.clear
                    }
                  }
                }

                Log.trace("[MEMDEBUG] (again) got the actor refSet... length: "+h.size)

              case _ =>
            }
          }
        } catch {
          case e => Log.error("[MEMDEBUG] failure", e)
        }
        ping()

      case _ =>
    }
  }

  private def ctor() {
    this.start
    ping()

  }

  private def ping() {
    ActorPing.schedule(this, "ActorBug", 1 minute)
  }

  ctor()
}

