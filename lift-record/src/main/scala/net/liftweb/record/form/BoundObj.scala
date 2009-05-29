/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.record.form

import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import scala.xml.{Node, NodeSeq, Text, Elem, Group, MetaData, Null, UnprefixedAttribute, PrefixedAttribute}

import net.liftweb.http.SHtml

trait BoundObjParam {}

class HtmlAttribute(val name: String, val value: String) extends BoundObjParam {
    override def toString = "A:"+name+"="+value
}

object HtmlAttribute {
    def apply(nameValue: (String, String)) = new HtmlAttribute(nameValue._1, nameValue._2)
}

class BoundObjConstuctor(
      val name: String
    , val addBoundObj:(String, BoundObj[Any]) => Unit
    , val setRebindMap: (String, String) => Unit
    , val getRebindMap: (String) => Option[String]
    , val params: List[BoundObjParam]
){}

abstract class BoundObj[A](boc: BoundObjConstuctor) {
    {boc.addBoundObj(boc.name, this.asInstanceOf[BoundObj[Any]])}

    val name = boc.name
    private[form] def setRebindMap = boc.setRebindMap
    private[form] def getRebindMap = boc.getRebindMap
    val htmlAttributes = boc.params.toList.filter(_.isInstanceOf[HtmlAttribute]).map(_.asInstanceOf[HtmlAttribute])
    val validatorParams = boc.params.toList.filter(_.isInstanceOf[Validator]).map(_.asInstanceOf[Validator])

    val getStringOpt: Option[String] = getRebindMap(name)

    def isEmpty = getStringOpt.getOrElse("") == ""

    def getOpt: Option[A]

    def getOrElse(orElse: => A): A = getOpt.getOrElse(orElse)
    
    private[form] def setRebind = (v: String) => setRebindMap(name, v)

    def :=(v: A) { setRebindMap(name, if (v == null) "" else mkString(v)) }

    def ::=(v: String) = { setRebindMap(name, if (v == null) "" else v) }
    
    def html: NodeSeq = {
        val attrs = htmlAttributes map (v => (v.name -> v.value) )
        net.liftweb.http.SHtml.text(getStringOpt.getOrElse(""), setRebind, attrs:_*)
    }

    def bindParam: BindParam = name -> html

    def stringBindParam: BindParam = name -> scala.xml.Text(getStringOpt.getOrElse(""))

    val defaultInvalidMsg = "Invalid"
    val defaultMandatoryMsg = "Required"
    def validations: List[Validator] = {
        var outList = validatorParams
        if (!outList.exists(_.errorType == "VALID")) outList = Validator.valid(defaultInvalidMsg) :: outList
        if (!outList.exists(v => v.errorType == "MANDATORY" || v.errorType == "OPTIONAL")) outList = Validator.mandatory(defaultMandatoryMsg) :: outList
        outList
    }

    def validate: List[ValidationError] = validations flatMap {_.validate(this.asInstanceOf[BoundObj[AnyRef]])}

    def mkValidationError(errStr: String): ValidationError = {
        new ValidationError(name, "ANYERR", errStr)
    }

    def mkString(obj: A) = if (obj == null) "" else obj.toString

    override def toString = getOpt match {case None => ""; case Some(other) => if (other == null) "" else mkString(other)}
}
