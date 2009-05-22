/*
 * Copyright 2009 WorldWide Conferencing, LLC
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

package net.liftweb.actor

import util._
import Helpers._

trait IExecute {
  def execute(f: () => Unit): Unit
  def shutdown(): Unit
}

object Scheduler {
  @volatile
  var onSameThread = false

  @volatile
  var createExecutor: () => IExecute = () => {
    new IExecute {
      import java.util.concurrent.{Executors, Executor}

      private val es: Executor = Executors.newCachedThreadPool()

      def execute(f: () => Unit): Unit =
      es.execute(new Runnable{def run() {f()}})


      def shutdown(): Unit = {}
    }
  }

  @volatile
  var exec: IExecute = _

  def execute(f: () => Unit) {touch; exec.execute(f)}

  private lazy val touch = {
    exec = createExecutor()
    true
  }
}

private[actor] trait IMailboxItem[T] {
  protected var next: IMailboxItem[T] = _
  protected var prev: IMailboxItem[T] = _

  private[actor] def find(f: IMailboxItem[T] => Boolean): Box[IMailboxItem[T]] =
  if (f(this)) Full(this) else next.find(f)

  private[actor] def remove() {
    val newPrev = prev
    prev.next = next
    next.prev = prev
  }

  private[actor] def insertAfter(newItem: IMailboxItem[T]) {
    next.prev = newItem
    newItem.prev = this
    newItem.next = this.next
    next = newItem
  }

  private[actor] def insertBefore(newItem: IMailboxItem[T]) {
    prev.next = newItem
    newItem.prev = this.prev
    newItem.next = this
    prev = newItem
  }

  def item: T
}

private[actor] class MailboxItem[T](val item: T) extends IMailboxItem[T]

trait SpecializeActor[T] extends IMailboxItem[T] {
  private var processing = false

  def !(msg: T): Unit = {
    val toDo: () => Unit = synchronized {
      addToMailbox(msg)
      if (!processing) {
        if (Scheduler.onSameThread) {
          processing = true
          () => processMailbox(true)
        } else {
          Scheduler.execute(() => processMailbox(false))
          () => {}
        }
      }
      else () => {}
    }
    toDo()
  }

  private def addToMailbox(msg: T): Unit = synchronized {
    this.insertBefore(new MailboxItem(msg))
  }

  def item = throw new NullPointerException("Trying to get an item where there is none")

  private def processMailbox(ignoreProcessing: Boolean) {
    val eh =
    synchronized {
      if (!ignoreProcessing && processing) return
      processing = true
      exceptionHandler
    }
    try {
      var done = false
      while (!done) {
        val pf = messageHandler
        synchronized{next.find(v => pf.isDefinedAt(v.item))} match {
          case Full(mb) =>
            synchronized {
              mb.remove()
            }
            try {
              pf(mb.item)
            } catch {
              case e: Exception => if (eh.isDefinedAt(e)) eh(e)
            }
          case _ => done = false
        }
      }
    } catch {
      case e =>
        if (eh.isDefinedAt(e)) eh(e)
        throw e
    } finally {
      synchronized {
        processing = false
      }
    }
  }

  override private[actor] def find(f: IMailboxItem[T] => Boolean): Box[IMailboxItem[T]] =
  Empty

  protected def messageHandler: PartialFunction[T, Unit]

  protected def exceptionHandler: PartialFunction[Throwable, Unit] = Map()

  private def ctor() {
    next = this
    prev = this
  }

  ctor()
}

trait Actor extends SpecializeActor[Any]
