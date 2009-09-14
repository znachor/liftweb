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

object Xml {
  import JsonAST._
  import scala.xml.{Elem, Node, NodeSeq}

  def toJson(xml: Node): JValue = {
    def childElems(n: Node) = n.child.filter(_.getClass == classOf[Elem])

    def build(root: NodeSeq, fieldName: Option[String], argStack: List[JValue]): List[JValue] = root match {
      case n: Node =>
        if (n.descendant.size == 1) JField(n.label, JString(n.text)) :: argStack
        else {
          val obj = JObject(build(childElems(n), Some(n.label), Nil).asInstanceOf[List[JField]])
          (fieldName match {
            case Some(n) => JField(n, obj)
            case None => obj
          }) :: argStack
        }
      case s: NodeSeq => 
        val allLabels = s.map(_.label)
        if (allLabels.size != 1 && allLabels.toList.removeDuplicates.size == 1) {
          val arr = JArray(s.flatMap(e => build(e, None, Nil)).toList)
          JField(allLabels(0), arr) :: argStack
        } else s.toList.flatMap(e => build(e, Some(e.label), Nil))
    }

    JObject(List(build(xml, Some(xml.label), Nil)(0).asInstanceOf[JField]))
  }
}
