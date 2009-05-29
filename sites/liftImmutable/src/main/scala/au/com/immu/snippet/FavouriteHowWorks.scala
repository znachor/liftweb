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

object FavouriteHowWorks {
    

    def show(xhtml: NodeSeq, cancelProc: () => Any): NodeSeq = {
        Log.debug ("FavouriteHowWorks.show")

        bind("favourite", xhtml, 
                   "cancel" -> submit( cancelProc ))
    }

}
