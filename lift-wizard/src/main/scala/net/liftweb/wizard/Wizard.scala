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
package net.liftweb.wizard

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml.{NodeSeq}
import _root_.java.util.Locale
import _root_.scala.collection.mutable.ListBuffer

object Wizard {


  trait Field  {
    def validate: List[FieldError]
    
    /**
     * Should this field appear on the confirmation page
     */
    def confirmPage_? = true

    def asBindParam: BindParam
  }
  
  object Field {
  
  }

  private object NextScreen extends RequestVar[Box[Screen]](Empty) {
    def unapply(x: Any): Option[Screen] = this.is
  }

  private object ScreenVars extends RequestVar[Map[String, (WizardVar[_], Any)]](Map())

  trait Screen extends DispatchSnippet {
    def dispatch = {
      case NextScreen(ns) => ns.screenContent
      case _ => screenContent
    }
    def templateName: Box[String] = Empty
    def locale: Locale = S.locale
    def template: NodeSeq = templateName.flatMap(s => TemplateFinder.findAnyTemplate(s.roboSplit("/"), locale)) openOr NodeSeq.Empty
    def nextScreen: Box[Screen] = Empty
    def finished: () => Unit = () => ()
    def fields: List[Field] = Nil
    def validate: List[FieldError] = fields.flatMap(_.validate)
    def howManyMore_? : Box[Int] = Empty
    def lastScreen_? = true
    def screenContent(in: NodeSeq) = bind(bindName, template, fields.map(_.asBindParam) :_*)
    def bindName = "wizard"
  }

  object Screen {

  }

  /**
   * Keep request-local information around without the nastiness of naming session variables
   * or the type-unsafety of casting the results.
   * RequestVars share their value through the scope of the current HTTP
   * request.  They have no value at the beginning of request servicing
   * and their value is discarded at the end of request processing.  They
   * are helpful to share values across many snippets.
   *
   * @param dflt - the default value of the session variable
   */
  abstract class WizardVar[T](dflt: => T) extends AnyVar[T, WizardVar[T]](dflt) {
    override protected def findFunc(name: String): Box[T] = WizardVarHandler.get(name)
    override protected def setFunc(name: String, value: T): Unit = WizardVarHandler.set(name, this, value)
    override protected def clearFunc(name: String): Unit = WizardVarHandler.clear(name)
    override protected def wasInitialized(name: String): Boolean = {
      val bn = name+"_inited_?"
      val old: Boolean = WizardVarHandler.get(bn) openOr false
      WizardVarHandler.set(bn, this, true)
      old
    }
  }

  trait CleanRequestVarOnSessionTransition {
    self: WizardVar[_] =>
  }

  private object WizardVarHandler /* extends LoanWrapper */ {
    //def vals = ScreenVars.is


    def get[T](name: String): Box[T] =
    ScreenVars.is.get(name).map(_._2.asInstanceOf[T])


    def set[T](name: String, from: WizardVar[_], value: T): Unit =
    ScreenVars.set(ScreenVars.is + (name -> (from, value)))

    def clear(name: String): Unit =
    ScreenVars.set(ScreenVars.is - name)
  }


  /*
   case class WizardPage[T] (
   val setup: (block: T => T),
   def next: Option[WizardPage[T],
   ) {
   def next_> (block: T => T) =
   new WizardPage(setup, Wizard(block))
   def choose...?
   }
   object Wizard { def apply(block: T => T) = WizardPage(block, None) }

   Wizard { t =>
   [bind snippets for first page]
   } next_> { t =>
   [second page]
   } choose {
   case t if t.something =>
   next_> { t =>
   [third if something]
   } next_> {
   [fourth, if something]
   } next_> {
   [fifth & last, if something]
   }
   case t =>
   next_> {
   [third and final if not something]
   }
   */
}

