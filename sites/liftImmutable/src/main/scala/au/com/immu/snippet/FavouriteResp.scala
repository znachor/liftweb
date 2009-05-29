package au.com.immu.snippet

import net.liftweb.http.S._

import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._

import net.liftweb.http.StatefulSnippet
import scala.xml.{Node, NodeSeq, Text, Elem, Group, MetaData, Null, UnprefixedAttribute, PrefixedAttribute}
import net.liftweb.mapper._
import net.liftweb.record.form._
import net.liftweb.util._
import net.liftweb.http.SHtml
import Favourite.FavouriteB

object FavouriteResp {
    
    def process(favouriteB: FavouriteB): FormReturn[Any] = {
        Log.debug ("FavouriteResp.process")
        FormReturn.success("/immu/favouriteHowWorks")
    }

    def show(xhtml: NodeSeq, favouriteB: FavouriteB, processProc: () => Unit, cancelProc: () => Unit): NodeSeq = {
        Log.debug ("FavouriteResp.show")

        def thoughts = {
            favouriteB.framework.get match {
                case FavouriteFramework.lift => "Thats such a good choice of framwork, we congratulate you on your cool choice"
                case _ => "We dont agree with your choice of framework, but, well were all allowed to have an opinion"
            }
        }

        def yourNameOrNickname = favouriteB.yourNickname.get match {
            case Some(nickname) => nickname
            case _ => favouriteB.yourName.get
        }

        /*
         favouriteB.stringBindParams binds text strings rather than the default html
         the binding can be overriden by including the name again (e.g. "donnation" --> ...)
         */
        bind("favourite", xhtml, favouriteB.stringBindParams,
                   "framework" -> favouriteB.framework.get.description,
                   "yourNameOrNickname" -> yourNameOrNickname,
                   "thoughts" -> thoughts,
                   "donation" -> favouriteB.donation.get.format,
                   "continue" -> submit( processProc ),
                   "cancel" -> submit( cancelProc ))
    }

}
