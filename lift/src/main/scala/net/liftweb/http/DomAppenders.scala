package net.liftweb.http

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.js._

private[http] object DomAppenders {
  def headAppenders(s: LiftSession): Node => Node =
    (ajaxAppend(s) _) andThen (cometHeadAppend(s) _)

  def bodyAppenders(s: LiftSession): Node => Node =
    (tailAppend((s)) _) andThen (liftGCAppend(s) _) andThen (cometAppend(s) _)
}

private[http] trait DomAppenders {
  def apply(session: LiftSession)(node: Node): Node

}

private[http] object tailAppend extends DomAppenders {
  def apply(session: LiftSession)(node: Node): Node =
    if (TailVar.get != NodeSeq.Empty) {
      Elem(node.prefix, node.label, node.attributes, node.scope,
          (node.child ++ HeadHelper.removeHtmlDuplicates(TailVar.get) ++ Text("\n")) :_*)
    } else node
}

/**
 * Appends comet stript to body
 */
private[http] object cometAppend extends DomAppenders {
  def apply(session: LiftSession)(node: Node): Node =
    CVPVar.is match {
      case list if !list.isEmpty && LiftRules.autoIncludeComet(session) =>
        Elem(node.prefix, node.label, node.attributes, node.scope,
            (node.child ++ JsCmds.Script(LiftRules.renderCometPageContents(session, list)) :_*))
      case _ => node
    }
}

/**
 * Appends comet stript reference to head
 */
private[http] object cometHeadAppend extends DomAppenders {
  def apply(session: LiftSession)(node: Node): Node =
    CVPVar.get match {
      case list if !list.isEmpty && LiftRules.autoIncludeComet(session) =>
        Elem(node.prefix, node.label, node.attributes, node.scope,
            (node.child ++ <script src={S.encodeURL("/"+
                                       	LiftRules.cometPath +
                                       	"/" + session.uniqueId +
                                       	"/" + LiftRules.cometScriptName())}
              							type="text/javascript"/>) :_*)
      case _ => node
    }
}

/**
 * Appends lift GC script to body
 */
private[http] object liftGCAppend extends DomAppenders {
  def apply(session: LiftSession)(node: Node): Node =
    if (LiftRules.enableLiftGC) {
      import js._
      import JsCmds._
      import JE._

      Elem(node.prefix, node.label, node.attributes, node.scope,
          (node.child ++ JsCmds.Script(OnLoad(JsRaw("liftAjax.lift_successRegisterGC()")) &
                                                JsCrVar("lift_page", RenderVersion.get)) :_*))
    } else node
}

 /**
  * Appends ajax stript to body
  */
private[http] object ajaxAppend extends DomAppenders {
  def apply(session: LiftSession)(node: Node): Node =
    if (LiftRules.autoIncludeAjax(session)) {
      Elem(node.prefix, node.label, node.attributes, node.scope,
          (node.child ++ <script src={S.encodeURL("/"+
                                  		LiftRules.ajaxPath +
                                  		"/" + LiftRules.ajaxScriptName())}
                                  		type="text/javascript"/>) :_*)
    } else node
}

