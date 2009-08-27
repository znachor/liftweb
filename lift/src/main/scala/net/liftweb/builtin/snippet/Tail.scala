package net.liftweb.builtin.snippet

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Box._

object Tail extends DispatchSnippet {

   def dispatch: DispatchIt = {
     case _ => render _
   }

   def render(xhtml: NodeSeq) : NodeSeq = <tail>xhtml</tail>
   /*{
     (for {ctx <- S.session ?~ ("FIX"+"ME: Invalid session")
          req <- S.request ?~ ("FIX"+"ME: Invalid request")
     } yield {
       val content = TailVar.get
       TailVar(content ++ ctx.processSurroundAndInclude(PageName.get, xhtml))
     }) match {
       case Full(x) => NodeSeq.Empty // Return an empty NodeSeq as this content will be appended later on
       case Empty => Comment("FIX"+ "ME: session or request are invalid")
       case Failure(msg, _, _) => Comment(msg)
     }
   }*/
}
