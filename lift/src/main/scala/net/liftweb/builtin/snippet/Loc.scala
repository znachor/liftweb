package net.liftweb.builtin.snippet

import scala.xml._
import net.liftweb.http._

class Loc extends DispatchSnippet {
  def dispatch : DispatchIt = {
    case "render" => render _
  }

  def render(kids: NodeSeq) : NodeSeq =
    S.attr.~("locid").map(_.text) match {
      case Some(id) => S.loc(id, kids)
      case _ => S.loc(kids.text, kids)
    }

}

