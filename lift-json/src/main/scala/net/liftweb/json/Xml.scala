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
  import scala.xml.{Elem, MetaData, Node, NodeSeq, Text, TopScope}

  def toJson(xml: NodeSeq): JValue = {
    def leaf_?(node: Node) = node.descendant.size == 1
    def array_?(nodeNames: Seq[String]) = nodeNames.size != 1 && nodeNames.toList.removeDuplicates.size == 1
    def childElems(n: Node) = n.child.filter(c => classOf[Elem].isAssignableFrom(c.getClass))
    def makeObj(name: String, f: => List[JValue]) = JObject(f map {
      case f: JField => f
      case x => JField(name, x)
    })
    def makeField(name: String, value: String) = JField(name, JString(value))
    def buildAttrs(n: Node) = n.attributes.map((a: MetaData) => makeField(a.key, a.value.text)).toList

    def build(root: NodeSeq, fieldName: Option[String], argStack: List[JValue]): List[JValue] = root match {
      case n: Node =>
        if (leaf_?(n)) makeField(n.label, n.text) :: argStack
        else {
          val obj = makeObj(n.label, buildAttrs(n) ::: build(childElems(n), Some(n.label), Nil))
          (fieldName match {
            case Some(n) => JField(n, obj)
            case None => obj
          }) :: argStack
        }
      case nodes: NodeSeq => 
        val allLabels = nodes.map(_.label)
        if (array_?(allLabels)) {
          val arr = JArray(nodes.toList.flatMap { n => 
            if (leaf_?(n)) JString(n.text) :: Nil
            else build(n, None, Nil) })
          JField(allLabels(0), arr) :: argStack
        } else nodes.toList.flatMap(n => build(n, Some(n.label), buildAttrs(n)))
    }
    
    (xml map { node => makeObj(node.label, build(node, None, Nil)) }).toList match {
      case List(x) => x
      case x => JArray(x)
    }
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
      case JNothing => Text("")
    }

    json match {
      case JField(n, v) => toXml(n, v)
      case JObject(fields) => fields flatMap { f => toXml(f.name, f.value) }
      case x => toXml("root", x)
    }
  }

  private[json] class XmlNode(name: String, children: Seq[Node]) extends Elem(null, name, xml.Null, TopScope, children :_*)

  private[json] class XmlElem(name: String, value: String) extends Elem(null, name, xml.Null, TopScope, Text(value))
}
