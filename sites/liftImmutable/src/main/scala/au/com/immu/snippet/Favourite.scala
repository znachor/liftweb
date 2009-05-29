package au.com.immu.snippet

import net.liftweb.http.S
import net.liftweb.http.S._

import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._

import net.liftweb.http.StatefulSnippet
import scala.xml.{Node, NodeSeq, Text, Elem, Group, MetaData, Null, UnprefixedAttribute, PrefixedAttribute}
import net.liftweb.mapper._

import net.liftweb.http.SHtml
import net.liftweb.record.form._
import net.liftweb.util._
import java.util.Date

/*  just a wrapper around Enumeration */
object FavouriteFramework extends EnumWithDescription {
    val lift = Value("lift", "Liftweb")
    val ruby = Value("ruby", "Ruby on Rails")
    val struts = Value("struts", "Struts 1 or 2")
    val other = Value("other", "Other")
}

/*
 Favourites
 */
object Favourite {

    /*
     The Binder object uses Lift's function mapping to set and get values from the client, but wraps Lift's functionality.

     When the FavouiteB class is instanciated, the objects within FavouriteB are bound to the current values stored in BinderMutable,
     and are immutable. For instance, if favouriteB is an instance of FavouriteB, favouriteB.yourName.get always returns the same String
    
     All bound objects are required, by default, and have a mandatory validation by default. If you want an
     optional value, for a String you can use bindStringOpt - yourNickName.get returns an Option[String]

     Objects such as donationDate, have a default validation requiring that they are Valid (i.e. can be converted from a string to the required type).
 
     */
    class FavouriteB extends Binder {

        // this is the default date format, but you can set it to whatever you want
        override val defaultDateFormat = new java.text.SimpleDateFormat("dd MMM yyyy")

        val yourName = bindString("yourName") // mandatory is the default for all bound object types
        val yourNickname = bindStringOpt("yourNickname") // optional string
        val framework = bindDroplistSelect[FavouriteFramework.Value]("framework", FavouriteFramework) // change the enumerations into a drop list
        val leastFramework = bindRadioOpt[FavouriteFramework.Value]("leastFramework", FavouriteFramework)// change the enumerations into a radio group
        val donation = bindUSD("donation", Validator.positive) // use a default value of 10 dollars, make sure they are giving not taking
        val donationDate = bindDate("donationDate") 
        val terms = bindCheckbox("terms") // the default is false

        def set {
            reset // clear out future bindings
            donation := US(10) // next time the class is instaniated, this will be the value
            donationDate := new java.util.Date()
            terms := false // false by default, but whatever
        }
    }
    def favouriteB = new FavouriteB

    def doProcess = {
        process(favouriteB)
    }

    def doReset {
        favouriteB.set
    }

    /*
     first, check the madatory, valid validations. If not valid return the validationErrors
     Otherwise, do any more complex validations. If everything is ok indicate where to go next
     */
    def process(favouriteB: FavouriteB): FormReturn[Any] = {
        Log.debug ("FavouriteB.process")
        import favouriteB._
        var validations = favouriteB.validate
        if (!terms.get) validations = terms.mkValidationError("Please accept the terms and conditions") :: validations

        validations match {
            case Nil =>
                val theName: String = yourName.get // its mandatory and valid, so its guaranteed to be a non null String
                val theDonationDate: Date = donationDate.get // stores a date, not a String
                val theNickName: Option[String] = yourNickname.get // its optional, so it returns an Option[String]

                // you can continue validation once you know the basic input is valid
                if (donation.get.toString == "0.00") FormReturn.fail(donation.name, "Please dontate more money")
                else FormReturn.success("/immu/favouriteResp")
            case validations =>
                Log.debug ("Register favourite account input not valid "+" "+validations)
                FormReturn.fail(validations)
        }
    }

    /*
     Get the xhtml to display, processProc and resetProc are provided by the controling class
     */
    def show(xhtml: NodeSeq, favouriteB: FavouriteB): NodeSeq = {
        Log.debug ("Favourite.show")

        /*
         SBind is a replacement for Bind, that also accepts Binder objects,
         allows lift binding via element name (e.g. name="favourite:continue")
         and mixes html- see favourite.html.
         */
        bind("favourite", xhtml, favouriteB.bindParams,
             "continue" -> submit( () => doProcess.continue ),
             "reset" -> submit( () => doReset ))

    }

}
