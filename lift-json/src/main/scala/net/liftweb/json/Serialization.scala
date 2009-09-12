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
 *  FIXME: add support to use java serialization for nonsupported types
 *  
 *  See: SerializationExamples.scala
 */
object Serialization {
  import Meta._

  def save[A <: AnyRef](a: A): String = {
    val mapping = mappingOf(a.getClass)

    // FIXME make this unncessary
    def pathOf(m: Mapping) = m match {
      case Value(p, _) => p
      case Constructor(Some(p), _, _) => p
      case _ => error("no path")
    }

    def serialize(mapping: Mapping): JValue = mapping match {
      case Value(path, targetType) => JInt(1)
      case Constructor(path, classname, args) => 
        JObject(args.map(a => JField(pathOf(a), serialize(a))))
/*
      case ListConstructor(path, classname, args) => 
        val arr = fieldValue(root, path).asInstanceOf[JArray]
        arr.arr.map(elem => newInstance(classname, args.flatMap(build(elem, _, argStack)))) :: argStack
      case ListOfPrimitives(path, elementType) =>
        val arr = fieldValue(root, path).asInstanceOf[JArray]
        arr.arr.map(elem => newPrimitive(elementType, elem)) :: argStack
      case Optional(m) =>
      */
    }

    Printer.compact(render(serialize(mapping)))
  }

  def load[A](json: String)(implicit mf: Manifest[A]) = parse(json).extract(DefaultFormats, mf)
}
