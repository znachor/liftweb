package net.liftweb.builtin.snippet

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.util._
import SHtml._
import JsCmds._
import JE._



object LazyLoad extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }
  
  def render(xhtml: NodeSeq): NodeSeq = {
    val id = "lazy_"+ Helpers.nextFuncName;
    
    val (name, exp) = ajaxInvoke(() => {
      S.session.map(session => Replace(id, xhtml)) openOr Noop
    })
    
    <tail>{Script(OnLoad(exp.cmd))}</tail> ++ <div id={id}></div>
  }
  
}
