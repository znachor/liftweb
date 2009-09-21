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
  import scala.xml.{Elem, Node, NodeBuffer, NodeSeq, Text}

  def toJson(xml: Node): JValue = {
    def childElems(n: Node) = n.child.filter(c => classOf[Elem].isAssignableFrom(c.getClass))

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

  def toXml(json: JValue): NodeSeq = {
    def toXml(name: String, json: JValue): NodeSeq = json match {
      case JObject(fields) => new XmlNode(name, fields flatMap { f => toXml(f.name, f.value) })
      case JArray(xs) => xs flatMap { v => toXml(name, v) }
      case JField(n, v) => new XmlNode(name, toXml(n, v))
      case JInt(x) => new XmlElem(name, x.toString)
      case JDouble(x) => new XmlElem(name, x.toString)
      case JString(x) => new XmlElem(name, x)
      case JBool(x) => new XmlElem(name, x.toString)
      case JNull => new XmlElem(name, "null")
      case JNothing => scala.xml.Comment("") // FIXME
    }

    json match {
      case JField(n, v) => toXml(n, v)
      case JObject(fields) => fields flatMap { f => toXml(f.name, f.value) }
      case x => toXml("root", x)
    }
  }

  private[json] class XmlNode(name: String, children: Seq[Node]) extends Elem(null, name, null, xml.TopScope, children :_*)

  private[json] class XmlElem(name: String, value: String) extends Elem(null, name, null, xml.TopScope, Text(value))
}
