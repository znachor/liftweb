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
 *  FIXME: @path
 *  FIXME: Option support
 *  FIXME: Map support
 * 
 *  See: SerializationExamples.scala
 */
object Serialization {
  import Meta.Reflection._

  val formats = DefaultFormats.lossless

  def save[A <: AnyRef](a: A): String = {
    def serialize(a: Any): JValue = a.asInstanceOf[AnyRef] match {
      case x if primitive_?(x.getClass) => primitive2jvalue(x)(formats)
      case x: List[_] => JArray(x map serialize)
      case x => 
        JObject(x.getClass.getDeclaredFields.filter(!static_?(_)).toList.map { f => 
          f.setAccessible(true)
          JField(f.getName, serialize(f get x))
        })
    }

    Printer.compact(render(serialize(a)))
  }

  def load[A](json: String)(implicit mf: Manifest[A]) = parse(json).extract(formats, mf)
}
