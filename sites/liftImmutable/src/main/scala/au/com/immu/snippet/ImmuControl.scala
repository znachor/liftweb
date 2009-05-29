package au.com.immu.snippet

import net.liftweb.http.SessionVar
import net.liftweb.http.RequestVar
import net.liftweb.http.S._
import net.liftweb.http.S
import net.liftweb.http.SHtml._
import net.liftweb.http.RequestVar
import net.liftweb.util.Helpers._
import net.liftweb.util.{Box, Empty, Failure, Full}
import net.liftweb.http.DispatchSnippet
import scala.xml.{Node, NodeSeq, Text, Elem, Group, MetaData, Null, UnprefixedAttribute, PrefixedAttribute}
import net.liftweb.mapper._
import net.liftweb.record.form._
import net.liftweb.util._
import net.liftweb.http.SHtml

// set up default values
object ImmuControlDefault {
    var setupBool = false
    def setup {
        if (!setupBool) Favourite.favouriteB.set
        setupBool = true
    }
}

/*
    Values are bound from html into immutable objects with a little bit of validation, pre done
 */
class ImmuControl extends DispatchSnippet {
    ImmuControlDefault.setup

    // dispatch the front end calls to the appropriate methods
    def dispatch: DispatchIt = _ match {
        case "favourite" => doFavourite _
        case "favouriteResp" => doFavouriteResp _
        case "favouriteHowWorks" => doFavouriteHowWorks _
        case "error" => doNothing _
        case x => {Log.error("------------FINDOUT "+x); whatami _}
    }

    def whatami(xhtml: NodeSeq): NodeSeq = <span>whatami</span>

    def doNothing(xhtml: NodeSeq): NodeSeq = xhtml

    /*
     Show the favourites
     pass a newly rebound FavouriteB objects to show and or process
     */
    def doFavourite(xhtml: NodeSeq): NodeSeq = {
        import Favourite._
        Log.debug ("doFavourite")

        Favourite.show(xhtml, favouriteB ) // use the favouriteB method to get newly bound immutable values
    }

    /*
     The favourites have been selected, show the response
     */
    def doFavouriteResp(xhtml: NodeSeq): NodeSeq = {
        import FavouriteResp._
        Log.debug ("doFavouriteResp")
        def doProcess() { process(Favourite.favouriteB).continue }

        FavouriteResp.show(xhtml, Favourite.favouriteB, doProcess, returnToFavourite )
    }
    
    /*
     How the binding works
     */
    def doFavouriteHowWorks(xhtml: NodeSeq): NodeSeq = {
        Log.debug ("doFavouriteHowWorks")

        FavouriteHowWorks.show(xhtml, returnToFavourite )
    }

    def returnToFavourite() {redirectTo("/immu/favourite")}


}
