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
import net.liftweb.util.{CurrencyZone, EnumWithDescription, AU, US}
import AU.AUD
import US.USD

import net.liftweb.http.SHtml
import java.util.Date
import java.text.SimpleDateFormat;
import java.util.Formatter
import net.liftweb.util.{Box, Empty, Failure, Full}
import FuncUtil.tryFunc
import net.liftweb.http.SessionVar
import net.liftweb.http.{FieldError, SHtml, FieldIdentifier}
import net.liftweb.record.Validator

class BinderMutable {
    var rebindMap = scala.collection.mutable.Map[String, String]()
}

object BinderMutableS extends SessionVar(scala.collection.mutable.Map[String, BinderMutable]())

abstract class Binder(binderMutableOpt: Option[BinderMutable]) {
    def this() = this(None)
    private[form] var boundObjList = List[BoundObj[Any]]()
    private[form] var binderList = List[Binder]()

    private[form] val binderMutable: BinderMutable = {
        def getSetBinderMutableOnSession = {

            val binderMutableMap = BinderMutableS.get
            val lastIndex = {
                this.getClass.toString.lastIndexOf(".") match {
                    case n if n > 0 => n+1
                    case _ => 0
                }
            }
            // use the class name as the key
            val theClassStr = this.getClass.toString.substring(lastIndex)
            // get/set the binderMutable from/to the session
            binderMutableMap.get(theClassStr) match {
                case None =>
                    val binderMutable = new BinderMutable
                    binderMutableMap(theClassStr) = binderMutable
                    binderMutable
                case Some(binderMutable) =>
                    binderMutable
            }
        }
        binderMutableOpt match {
            case None => getSetBinderMutableOnSession
            case Some(bm) => bm
        }
    }

    // if you have a collection of a binder type, you can use the index to make
    // the boundObj name unique, otherwise pass a distinct binderMutable in the constructor
    def index = ""

    def reset {
        boundObjList foreach {boundObj => boundObj := null}
        binderList foreach {_.reset}
    }

    def addBoundObj(boundObjName: String, boundObj: BoundObj[Any]) {
        boundObjList = boundObj :: (boundObjList.filter(_.name != boundObjName) )
    }

    def getRebindMap(name: String): Option[String] = {
        def ltrim(str: String, chr: Char) = str.replaceAll("^["+chr.toString+"]+", "")
        def rtrim(str: String, chr: Char) = str.replaceAll("["+chr.toString+"]+$", "")
        val amt = ltrim(rtrim(binderMutable.rebindMap.get(name+index).getOrElse("")  , ' '), ' ')

        amt match {
            case "" => None
            case nonNullStr => Some(nonNullStr)
        }
    }

    def setRebindMap(name: String, str: String) {
        binderMutable.rebindMap += ((name+index) -> str)
    }

    def validate: List[FieldError] = (boundObjList flatMap (_.validate) ) ::: ( binderList flatMap (_.validate) )

    def bindParams: List[BindParam] = ( boundObjList map (_.bindParam) ) ::: ( binderList flatMap (_.bindParams)  )

    def stringBindParams: List[BindParam] = ( boundObjList map (_.stringBindParam) ) ::: ( binderList flatMap (_.stringBindParams)  )

    def bindBinder[A <: Binder](binder: A) = { binderList = binder :: binderList ; binder}

    def boc(name: String, vt: List[BoundObjParam]): BoundObjConstuctor = {
        new BoundObjConstuctor(name, addBoundObj, setRebindMap, getRebindMap, vt)
    }

    def addOpt(vt: List[BoundObjParam]) = Validator.optional :: vt

    def bindString(name: String, vt: BoundObjParam*): BoundString =
    new BoundString(boc(name, vt.toList))

    def bindStringOpt(name: String, vt: BoundObjParam*) =
    new BoundStringOpt(boc(name, addOpt(vt.toList)) )

    def bindPassword(name: String, vt: BoundObjParam*): BoundString =
    new BoundString(boc(name, HtmlAttribute("type", "password") :: vt.toList))

    def bindPasswordOpt(name: String, vt: BoundObjParam*) =
    new BoundStringOpt(boc(name, HtmlAttribute("type", "password") :: addOpt(vt.toList)) )

    def bindAUD(name: String, vt: BoundObjParam*) =
    new BoundMoney(AU, AU(0), boc(name, addOpt(vt.toList)) )

    def bindAUDOpt(name: String, vt: BoundObjParam*) =
    new BoundMoneyOpt(AU, AU(0), boc(name, addOpt(vt.toList)))

    def bindUSD(name: String, vt: BoundObjParam*) =
    new BoundMoney(US, US(0), boc(name, vt.toList))

    def bindUSDOpt(name: String, vt: BoundObjParam*) =
    new BoundMoneyOpt(US, US(0), boc(name, addOpt(vt.toList)))

    def bindMoney[A <: CurrencyZone#AbstractCurrency](currency: CurrencyZone, zero: A, name: String, vt: BoundObjParam*) =
    new BoundMoney[A](currency, zero, boc(name, vt.toList))

    def bindMoneyOpt[A <: CurrencyZone#AbstractCurrency](currency: CurrencyZone, zero: A, name: String, vt: BoundObjParam*) =
    new BoundMoneyOpt(currency, zero: A, boc(name, addOpt(vt.toList)) )

    val defaultDateFormat = new java.text.SimpleDateFormat("dd MMM yyyy")

    def bindDate(name: String, vt: BoundObjParam*) =
    new BoundDate(boc(name, vt.toList), defaultDateFormat)

    def bindDate(name: String, dateFormat: SimpleDateFormat, vt: BoundObjParam*)=
    new BoundDate(boc(name, vt.toList), dateFormat)

    def bindDateOpt(name: String, vt: BoundObjParam*) =
    new BoundDateOpt(boc(name, addOpt(vt.toList)), defaultDateFormat)

    def bindDateOpt(name: String, dateFormat: SimpleDateFormat, vt: BoundObjParam*) =
    new BoundDate(boc(name, addOpt(vt.toList)), dateFormat)

    def bindCheckbox(name: String, vt: BoundObjParam*) =
    new BoundCheckBox(boc(name, vt.toList))

    def bindDroplist[A](name: String, enum: EnumWithDescription, vt: BoundObjParam*) =
    new BoundDropList[A](boc(name, vt.toList), None, None, enum)

    def bindDroplistSelectOpt[A](name: String, enum: EnumWithDescription, vt: BoundObjParam*) =
    new BoundDropListOpt[A](boc(name, addOpt(vt.toList)), Some("select"), None, enum)

    def bindDroplistSelect[A](name: String, enum: EnumWithDescription, vt: BoundObjParam*) =
    new BoundDropList[A](boc(name, vt.toList), Some("select"), None, enum)

    def bindDroplistSelect[A](name: String, enum: EnumWithDescription, filter: String => Boolean, vt: BoundObjParam*) =
    new BoundDropList[A](boc(name, vt.toList), Some("select"), Some(filter), enum)

    def bindInteger(name: String, vt: BoundObjParam*) =
    new BoundNumber[Integer](boc(name, vt.toList), Integer.parseInt(_))

    def bindIntegerOpt(name: String, vt: BoundObjParam*) =
    new BoundNumberOpt[Integer](boc(name, addOpt(vt.toList)), Integer.parseInt(_))

    def bindDouble(name: String, vt: BoundObjParam*) =
    new BoundNumber[Double](boc(name, vt.toList), java.lang.Double.parseDouble(_))

    def bindDoubleOpt(name: String, vt: BoundObjParam*) =
    new BoundNumberOpt[Double](boc(name, addOpt(vt.toList)), java.lang.Double.parseDouble(_))

    def bindLong(name: String, vt: BoundObjParam*) =
    new BoundNumber[Long](boc(name, vt.toList), java.lang.Long.parseLong(_))

    def bindLongOpt(name: String, vt: BoundObjParam*) =
    new BoundNumberOpt[Long](boc(name, addOpt(vt.toList)), java.lang.Long.parseLong(_))

    def bindRadio[A](name: String, enum: EnumWithDescription, vt: BoundObjParam*) =
    new BoundRadio[A](boc(name, vt.toList), enum, radioHtmlBr)

    def bindRadioOpt[A](name: String, enum: EnumWithDescription, vt: BoundObjParam*) =
    new BoundRadioOpt[A](boc(name, addOpt(vt.toList)), enum, radioHtmlBr)

    def bindRadioOpt[A](name: String, enum: EnumWithDescription, addHtml: RadioConstructor => NodeSeq, vt: BoundObjParam*) =
    new BoundRadioOpt[A](boc(name, addOpt(vt.toList)), enum, addHtml)

    def radioHtmlInLine(constructor: RadioConstructor) = {
        radioFragHtml(constructor) ++ radioFragLabel(constructor)
    }

    def radioHtmlBr(constructor: RadioConstructor) = {
        radioFragHtml(constructor) ++ radioFragLabel(constructor) ++ <br/>
    }

    def radioFragLabel(constructor: RadioConstructor) = <label class="radiol" for={constructor.enumName}>{constructor.enumDesc}</label>

    def radioFragHtml(constructor: RadioConstructor) = constructor.curVal match {
        case Some(curEnumName) if (constructor.enumName == curEnumName) =>
            <input class="radiol" type="radio" checked="checked" id={constructor.enumName} name={constructor.liftName} value={constructor.enumName} />
        case _ =>
            <input class="radiol" type="radio" id={constructor.enumName} name={constructor.liftName} value={constructor.enumName} />
    }

    /*def mkError(name: String, errStr: String): ValidationError = {
        new ValidationError(name, "ANYERR", errStr)
    }*/

}

class RadioConstructor (
    val liftName: String,
      val curVal: Option[String],
      val enumName: String,
      val enumDesc: String){}

class BoundNumber[A](boc: BoundObjConstuctor, getFromStr: String => A)
extends BoundNumberBase[A](boc, getFromStr) {def get = getOpt.getOrElse(null).asInstanceOf[A]}
class BoundNumberOpt[A](boc: BoundObjConstuctor, getFromStr: String => A)
extends BoundNumberBase[A](boc, getFromStr) {def get = getOpt.getOrElse(null).asInstanceOf[Option[A]]}
abstract class BoundNumberBase[A](boc: BoundObjConstuctor, getFromStr: String => A) extends BoundObj[A](boc) {
    override val defaultInvalidMsg = "Invalid Number"
    def getOpt: Option[A] =  tryFunc( getFromStr(getStringOpt.getOrElse("")) )
}

class BoundString(boc: BoundObjConstuctor) extends BoundStringBase(boc) {def get = getOpt.getOrElse(null)}
class BoundStringOpt(boc: BoundObjConstuctor) extends BoundStringBase(boc) {def get = getOpt}
abstract class BoundStringBase(boc: BoundObjConstuctor) extends BoundObj[String](boc) {
    def getOpt: Option[String] = getStringOpt
}

class BoundMoney[A <: CurrencyZone#AbstractCurrency](currency: CurrencyZone, zero: A, boc: BoundObjConstuctor)
extends BoundMoneyBase[A](boc, zero, currency) {def get = getOpt.getOrElse(null).asInstanceOf[A]}
class BoundMoneyOpt[A <: CurrencyZone#AbstractCurrency](currency: CurrencyZone, zero: A, boc: BoundObjConstuctor)
extends BoundMoneyBase[A](boc, zero, currency) {def get = getOpt.asInstanceOf[Option[A]]}
abstract class BoundMoneyBase[A <: CurrencyZone#AbstractCurrency](boc: BoundObjConstuctor, zero: A, currency: CurrencyZone) extends BoundObj[A](boc) {
    override val defaultInvalidMsg = Validator.defaultAmountValid.get
    override val validatePositive = Some(Validator.fnValidator[A]("gtEQ_0", false, (x: A) => {if (x.amount < zero.amount) Full(Text("Enter a positive amount")) else Empty}))

    def getOpt: Option[A] = {
        getStringOpt match {case None => None; case Some(str) => tryFunc( currency.apply(str.toString.replaceAll(",", "")).asInstanceOf[A] ) }
    }
}


class BoundDate(boc: BoundObjConstuctor, inDateFormat: SimpleDateFormat) extends BoundDateBase(boc, inDateFormat) {def get = getOpt.getOrElse(null)}
class BoundDateOpt(boc: BoundObjConstuctor, inDateFormat: SimpleDateFormat) extends BoundDateBase(boc, inDateFormat) {def get = getOpt}
abstract class BoundDateBase(boc: BoundObjConstuctor, inDateFormat: SimpleDateFormat) extends BoundObj[Date](boc) {
    override val defaultInvalidMsg = "Invalid Date"
    val dateFormat = inDateFormat
    dateFormat.setLenient(false)

    def getADate(str: String) = {
        val theDate = dateFormat.parse(str)
        val theString = dateFormat.format(theDate)
        if (theString != str) {
            throw new RuntimeException("grotty date")
        }
        theDate
    }
    def getOpt: Option[Date] = getStringOpt match {case None => None; case Some(str) =>  tryFunc( getADate(str) ) }
    override def mkString(obj: Date) = obj match {
        case null => ""
        case date => dateFormat format (date)
    }

    def > (date: Date) = {
        getOpt match {
            case None => false
            case Some(leftDate) =>
                val rightDate = getADate(mkString(date))
                leftDate after rightDate
        }
    }

    def < (date: Date) = ! >(date)

    def == (date: Date) = {
        getOpt match {
            case None => false
            case Some(leftDate) =>
                val rightDate = getADate(mkString(date))
                leftDate equals rightDate
        }
    }
}

class BoundCheckBox(boc: BoundObjConstuctor) extends BoundObj[Boolean](boc) {
    def getOpt: Option[Boolean] = getStringOpt match {case Some("true") => Some(true); case _ => Some(false)}
    override def html: NodeSeq = {
        val attrs = htmlAttributes map (v => (v.name -> v.value) )
        net.liftweb.http.SHtml.checkbox(get,  (v: Boolean) => boc.setRebindMap(name, v.toString), attrs:_*)
    }
    def get: Boolean = getOpt.getOrElse(false)
}


class BoundDropList[A](boc: BoundObjConstuctor, first: Option[String], filter: Option[String => Boolean], enum: EnumWithDescription)
extends BoundDropListBase[A](boc, first, filter, enum) {def get = getOpt.getOrElse(null).asInstanceOf[A]}
class BoundDropListOpt[A](boc: BoundObjConstuctor, first: Option[String], filter: Option[String => Boolean], enum: EnumWithDescription)
extends BoundDropListBase[A](boc, first, filter, enum) {def get = getOpt.asInstanceOf[Option[A]]}
abstract class BoundDropListBase[A](boc: BoundObjConstuctor, first: Option[String], filter: Option[String => Boolean], enum: EnumWithDescription) extends BoundObj[A](boc) {
    override val getStringOpt: Option[String] = {
        getRebindMap(name) match {
            case Some(str) if (Some(str) == first) => None
            case other => other
        }
    }
    def getOpt: Option[A] = {
        tryFunc( enum.valueOf(getStringOpt.getOrElse(null)) ) match {
            case Some(Some(x)) => Some(x).asInstanceOf[Option[A]]
            case _ => None
        }
    }

    override def ::=(in: String) {
        tryFunc( enum.valueOf(in) ) match {
            case Some(Some(x)) => setRebindMap(name, x.toString)
            case _ => None
        }
    }

    override def html: NodeSeq = {
        import net.liftweb.util.Box._
        def nameDescriptionList = {
            filter match {
                case None => enum.nameDescriptionList
                case Some(fn) => enum.nameDescriptionList filter (v => fn(v._1))
            }

        }
        val attrs = htmlAttributes map (v => (v.name -> v.value) )

        def addOptFirst = first match {
            case None => nameDescriptionList
            case Some(name) => (name -> name) :: nameDescriptionList
        }

        def getSelectBox = getOpt match {case None=>Empty; case Some(x)=>Full(x.toString)}

        select(addOptFirst, getSelectBox, setRebind, attrs:_*)

    }

}

class BoundRadio[A](boc: BoundObjConstuctor, enum: EnumWithDescription, radioHtml: RadioConstructor => NodeSeq)
extends BoundRadioBase[A](boc, enum, radioHtml) {def get = getOpt.getOrElse(null).asInstanceOf[A]}
class BoundRadioOpt[A](boc: BoundObjConstuctor, enum: EnumWithDescription, radioHtml: RadioConstructor => NodeSeq)
extends BoundRadioBase[A](boc, enum, radioHtml) {def get = getOpt.asInstanceOf[Option[A]]}
abstract class BoundRadioBase[A](boc: BoundObjConstuctor, enum: EnumWithDescription, radioHtml: RadioConstructor => NodeSeq)
extends BoundDropListBase[A](boc, None, None, enum) {

    override def html: NodeSeq = {
        import net.liftweb.util.Box._

        val sFuncHolder = SFuncHolder(setRebind)
        val liftName = fmapFunc[String](sFuncHolder)(x => x)
        def getHtml(enumName: String) = getStringOpt match {
            case Some(enumVal) if (enumName == enumVal) => <input type="radio" checked="checked" id={enumName} name={name} value={enumName} />
            case _ => <input type="radio" id={enumName} name={name} value={enumName} />
        }

        val radioList = enum.nameDescriptionList map ((v) => radioHtml(new RadioConstructor(liftName, getStringOpt, v._1, v._2) ) )
        var out: NodeSeq = Nil
        radioList.foreach(v => out = out ++ v)
        out

    }
}