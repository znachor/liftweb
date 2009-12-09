/*
 * Copyright 2006-2009 WorldWide Conferencing, LLC
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
package net.liftweb.http.xml

import _root_.net.liftweb.{common, http, util}
import common.{Box,Full,Empty,Failure}
import http.TemplateFinder.findAnyTemplate
import util.Props
import scala.xml.{NodeSeq, Text}

object Bindings {
    type Binding = NodeSeq => NodeSeq

    type DataBinding[T] = T => NodeSeq => NodeSeq

    //adds a bind() function to an object if an implicit DataBinding is available for that object
    implicit def binder[T](t: T)(implicit binding: DataBinding[T]): Binder = Binder(binding(t))
    implicit def binder(binding: Binding): Binder = Binder(binding)

    //decorator for a binding function that allows it to be called as bind() rather than apply()
    //also provides facilities for binding to a specific template
    case class Binder(val binding: Binding) {
        def bind(xhtml: NodeSeq): NodeSeq = binding.apply(xhtml)

        def bind(templatePath: List[String]): NodeSeq = {
            findAnyTemplate(templatePath) map binding match {
                case Full(xhtml) => xhtml
                case Failure(msg, ex, _) if Props.mode == Props.RunModes.Development => Text(ex.map(_.getMessage).openOr(msg))
                case Empty if Props.mode == Props.RunModes.Development => Text("Unable to find template with path " + templatePath.mkString("/", "/", ""))
                case _ => NodeSeq.Empty
            }
        }
    }

    object EmptyBinding extends Binding {
        override def apply(xhtml : NodeSeq) : NodeSeq = NodeSeq.Empty
    }
}

