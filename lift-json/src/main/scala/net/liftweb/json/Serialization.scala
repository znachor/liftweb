package net.liftweb.json

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

import java.lang.reflect.{Constructor => JConstructor, Type}
import java.util.Date
import scala.reflect.Manifest
import JsonAST._
import JsonParser.parse

/** Functions to serialize and deserialize a case class.
 *
 *  FIXME: add Map support
 * 
 *  See: SerializationExamples.scala
 */
object Serialization {
  import java.io.{StringWriter, Writer}
  import Meta.unmangleName
  import Meta.Reflection._

  val formats = DefaultFormats.lossless

  def write[A <: AnyRef](a: A): String = write(a, new StringWriter).toString

  def write[A <: AnyRef, W <: Writer](a: A, out: W): W = {
    def serialize(a: Any): JValue = a.asInstanceOf[AnyRef] match {
      case null => JNull
      case x if primitive_?(x.getClass) => primitive2jvalue(x)(formats)
      case x: List[_] => JArray(x map serialize)
      case x: Option[_] => serialize(x getOrElse JNothing)
      case x => 
        x.getClass.getDeclaredFields.filter(!static_?(_)).toList.map { f => 
          f.setAccessible(true)
          JField(unmangleName(f), serialize(f get x))
        } match {
          case Nil => JNothing
          case fields => JObject(fields)
        }
    }

    Printer.compact(render(serialize(a)), out)
  }

  def read[A](json: String)(implicit mf: Manifest[A]): A = parse(json).extract(formats, mf)
}
