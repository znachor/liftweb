/*
 * Copyright 2006-2009 WorldWide Conferencing, LLC
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
package net.liftweb.record

import _root_.scala.xml.{Node, Text}
import _root_.java.util.regex._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.S

trait BoundObjParam {}

abstract class Validator[A](val errorType: String) extends BoundObjParam {
    def validate(in: Box[A]): Box[Node]
    override def toString = "V:"+errorType
}

class DefaultMsg(val resourcePath: Option[String], orElseMsg: String) {
    private var msgOpt: Option[String] = None
    def get = msgOpt match {
        case None => orElseMsg
        case Some(msg) => msg
    }
    def set(msg: String) {msgOpt = Some(msg)}
}

object Validator {

    val defaultMandatory = new DefaultMsg(None, "Required")
    val defaultAmountValid = new DefaultMsg(None, "Invalid Amount")

    def fnValidator[A](errorType: String, emptyAsNull: Boolean, fn: A => Box[Node]) = {
        new Validator[A](errorType) {
            def validate(in: Box[A]) = in match {
                case Empty if emptyAsNull => fn(null.asInstanceOf[A])
                case Full(x) => fn(x)
                case _ => Empty
            }
        }
    }

    implicit def fnToValidator[A](fn: A => Box[Node]): Validator[A] = fnValidator("fnType", true, fn)

    def mandatory[A](errStr: String) = new Validator[A]("mandatory") {
        def validate(in: Box[A]) = in match {
            case Empty => Full(Text(errStr))
            case _ => Empty
        }
    }

    def optional[A] = new Validator[A]("optional") {def validate(in: Box[A]) = Empty}

    def valid[A](errStr: String) = new Validator[A]("valid") {
        def validate(in: Box[A]) = in match {
            case Failure(_,_,_) => Full(Text(errStr))
            case _ => Empty
        }
    }

    def postitveOrNegative[A] = new Validator[A]("postitveOrNegative") {def validate(in: Box[A]) = Empty}

    def compare[A <% Ordered[A]](errorType: String, errStr: String, fn: A => Boolean) = new Validator[A](errorType) {
        def validate(in: Box[A]) = in match {
            case Full(x) if fn(x) => Full(Text(errStr))
            case _ => Empty
        }
    }

    def gt[A <% Ordered[A]](gtAmount: A, errStr: String) = compare[A]("gt_"+gtAmount, errStr, (x: A) => x > gtAmount)
    def gtEQ[A <% Ordered[A]](gtAmount: A, errStr: String) = compare[A]("gtEQ_"+gtAmount, errStr, (x: A) => x >= gtAmount)
    def lt[A <% Ordered[A]](gtAmount: A, errStr: String) = compare[A]("lt_"+gtAmount, errStr, (x: A) => x < gtAmount)
    def ltEQ[A <% Ordered[A]](gtAmount: A, errStr: String) = compare[A]("ltEQ_"+gtAmount, errStr, (x: A) => x <= gtAmount)
    def range[A <% Ordered[A]](lower: A, upper: A, errStr: String) = compare[A]("range_"+lower+"_"+upper, errStr, (x: A) => x >= lower && x < upper)

}

