/*
 * Binding.scala
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

