/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package util {

import _root_.java.util.regex.Pattern
import _root_.net.liftweb.common.{Box, Full}
import _root_.scala.xml.{NodeSeq, Text}

trait HasMaxLen {
  def maxLen: Int
}

trait StringFieldHelpers {
  self: FieldIdentifier with HasMaxLen =>

  final def crop(in: String): String = in.substring(0, Math.min(in.length, maxLen))

  final def removeRegExChars(regEx: String)(in: String): String = in.replaceAll(regEx, "")

  final def toLower(in: String): String = in match {
    case null => null
    case s => s.toLowerCase
  }
  final def toUpper(in: String): String = in match {
    case null => null
    case s => s.toUpperCase
  }

  final def trim(in: String): String = in match {
    case null => null
    case s => s.trim
  }

  final def notNull(in: String): String = in match {
    case null => ""
    case s => s
  }


  /**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  def valMinLen(len: Int, msg: => String)(value: String): List[FieldError] =
  if ((value eq null) || value.length < len) List(FieldError(this, Text(msg)))
  else Nil

  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  def valMaxLen(len: Int, msg: => String)(value: String): List[FieldError] =
  if ((value ne null) && value.length > len) List(FieldError(this, Text(msg)))
  else Nil

  /**
   * Make sure the field matches a regular expression
   */
  def valRegex(pat: Pattern, msg: => String)(value: String): List[FieldError] = pat.matcher(value).matches match {
    case true => Nil
    case false => List(FieldError(this, Text(msg)))
  }

  final def cropBox(in: Box[String]): Box[String] = in.map(in => in.substring(0, Math.min(in.length, maxLen)))

  final def removeRegExCharsBox(regEx: String)(in: Box[String]): Box[String] = in.map(_.replaceAll(regEx, ""))

  final def toLowerBox(in: Box[String]): Box[String] = in.map {
    case null => null
    case s => s.toLowerCase
  }

  final def toUpperBox(in: Box[String]): Box[String] = in map {
    case null => null
    case s => s.toUpperCase
  }

  final def trimBox(in: Box[String]): Box[String] = in map {
    case null => null
    case s => s.trim
  }

  final def notNullBox(in: Box[String]): Box[String] = in match {
    case Full(x) if null eq x => Full("")
    case Full(x) => Full(x)
    case _ => Full("")
  }


  /**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  def valMinLenBox(len: Int, msg: => String)(value: Box[String]): List[FieldError] =
  value match {
    case Full(value) => if ((value eq null) || value.length < len) List(FieldError(this, Text(msg))) else Nil
    case _ => List(FieldError(this, Text(msg)))
  }

  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  def valMaxLenBox(len: Int, msg: => String)(value: Box[String]): List[FieldError] =
  for {
    v <- value.toList
    ret <-  if ((v ne null) && v.length > len) List(FieldError(this, Text(msg))) else Nil
  } yield ret


  /**
   * Make sure the field matches a regular expression
   */
  def valRegexBox(pat: Pattern, msg: => String)(value: Box[String]): List[FieldError] = value match {
    case Full(value) =>
      pat.matcher(value).matches match {
        case true => Nil
        case false => List(FieldError(this, Text(msg)))
      }
    case _ => List(FieldError(this, Text(msg)))
  }
}

}
}
