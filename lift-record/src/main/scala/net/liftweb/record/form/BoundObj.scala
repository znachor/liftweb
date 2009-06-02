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
import net.liftweb.util._
import net.liftweb.http.{FieldError, SHtml, FieldIdentifier}
import net.liftweb.record.Validator
import net.liftweb.record.BoundObjParam
import net.liftweb.http.SHtml

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
    val fieldIdentifier = new FieldIdentifier {
        override def uniqueFieldId = Full(boc.name) //todo addIndex
        override def toString = name
    }
    private[form] def setRebindMap = boc.setRebindMap
    private[form] def getRebindMap = boc.getRebindMap
    val htmlAttributes = boc.params.toList.filter(_.isInstanceOf[HtmlAttribute]).map(_.asInstanceOf[HtmlAttribute])
    val validatorParams = boc.params.toList.filter(_.isInstanceOf[Validator[A]]).map(_.asInstanceOf[Validator[A]])

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

    val validatePositive: Option[Validator[A]] = None
    val defaultInvalidMsg = "Invalid"
    val defaultMandatoryMsg = Validator.defaultMandatory.get
    val defaultPositiveMsg = "Positive values only"
    def validations: List[Validator[A]] = {
        var outList = validatorParams
        if (!outList.exists(_.errorType == "valid")) {
            outList = Validator.valid[A](defaultInvalidMsg) :: outList
        }
        if (!outList.exists(v => v.errorType == "mandatory" || v.errorType == "optional")) {
            outList = Validator.mandatory[A](defaultMandatoryMsg) :: outList
        }
        validatePositive match {
            case Some(v) if (!outList.exists(v => v.errorType == "gtEQ_0" || v.errorType == "postitveOrNegative"))  =>
                outList = v :: outList
            case _ => ;

        }
        outList
    }

    def validate: List[FieldError] = {
        val box = getOpt match {
            case None if (isEmpty) => Empty
            case None => Failure(getStringOpt.getOrElse(""), Empty, Empty)
            case Some(x) => Full(x)
        }
        validations flatMap {v =>
            v.validate(box) match {

                case Full(node) =>
                    println("field="+fieldIdentifier+" value="+getStringOpt+" "+v.errorType+"="+node)
                    new FieldError(fieldIdentifier, node) :: Nil
                case x =>
                    println("field="+fieldIdentifier+" value="+getStringOpt+" "+v.errorType+"="+x)
                    Nil
            }
        }
    }

    def mkError(errStr: String): FieldError = {
        new FieldError(fieldIdentifier, Text(errStr))
    }

    def mkString(obj: A) = if (obj == null) "" else obj.toString

    override def toString = getOpt match {case None => ""; case Some(other) => if (other == null) "" else mkString(other)}
}
