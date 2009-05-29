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
import net.liftweb.mapper._
import java.util.regex._
import net.liftweb.http.SHtml

// indicate a validation error has occured
sealed case class ValidationError (val bindName: String, val errorType: String, val msg: String) {
    def mkLiftError {
        if (bindName == null) error(msg)
        else error(bindName, msg)
    }
}

abstract class Validator(val errorType: String) extends BoundObjParam {
    def validate(bound: BoundObj[AnyRef]): List[ValidationError]
    override def toString = "V:"+errorType
}

object Validator {

    def mkError(bindName: String, msgString: String) = {
        ValidationError(bindName, null, msgString)
    }
    
    def isEmpty(str: AnyRef) = str == null || str.toString.replaceAll(" ", "") == ""

    def validEmail(in: String) = {
        val emailPattern = Pattern.compile("^[a-z0-9._%-]+@(?:[a-z0-9-]+\\.)+[a-z]{2,4}$")
        if (in == null) false
        else emailPattern.matcher(in.toLowerCase) matches
    }

    def email: Validator = email("Invalid email")
    def email(errStr: String) = new Validator("EMAIL") {
        def validate(bound: BoundObj[AnyRef]) = {
            if (isEmpty(bound.getStringOpt.getOrElse(null)) || validEmail(bound.toString)) Nil
            else new ValidationError(bound.name, errorType, errStr) :: Nil
        }
    }

    def optional = new Validator("OPTIONAL") {
        def validate(bound: BoundObj[AnyRef]) = Nil
    }

    def mandatory(errStr: String) = new Validator("MANDATORY") {
        def validate(bound: BoundObj[AnyRef]) = {
            if (!isEmpty(bound.getStringOpt.getOrElse(null)) ) Nil
            else new ValidationError(bound.name, errorType, errStr) :: Nil
        }
    }


    def valid(errStr: String) = new Validator("VALID") {
        def validate(bound: BoundObj[AnyRef]) = {
            if (isEmpty(bound.getStringOpt.getOrElse(null)) || bound.getOpt.getOrElse(null) != null) Nil
            else new ValidationError(bound.name, errorType, errStr) :: Nil
        }
    }

    def replaceCommaAndDollar(in: String): String = in.replaceAll("$", "").replaceAll(",", "")

    def positive = new Validator("POSITIVE") {
        def validate(bound: BoundObj[AnyRef]) = {
            if (isEmpty(bound.toString) || (replaceCommaAndDollar(""+bound.getOrElse("0"))).toDouble >= 0 ) Nil
            else new ValidationError(bound.name, errorType, "Positive values only") :: Nil
        }
    }

    def Range(lower: Int, upper: Int, errStr: String) = new Validator("RANGE") {
        def validate(bound: BoundObj[AnyRef]) = {
            if (isEmpty(bound.toString)) Nil
            else FuncUtil.tryFunc( (replaceCommaAndDollar(""+bound.getOrElse("0"))).toDouble) match {
                case Some(d) if (d >= lower && d <= upper) => Nil
                case _ => new ValidationError(bound.name, errorType, errStr) :: Nil
            }
        }
    }
    
    def isTrue(func:(BoundObj[AnyRef]) => Boolean, errStr: String) = new Validator("ANY_VALIDATOR") {
        def validate(bound: BoundObj[AnyRef]) = {
            if (isEmpty(bound.toString)) Nil
            else FuncUtil.tryFunc(func(bound)) match {
                case Some(d) if (d) => Nil
                case _ => new ValidationError(bound.name, errorType, errStr) :: Nil
            }
        }
    }
}