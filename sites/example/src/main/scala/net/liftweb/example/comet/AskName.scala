package net.liftweb.example.comet

import net.liftweb.http._
import S._
import net.liftweb.util._
import scala.xml._

class AskName(theSession: LiftSession, name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]) extends 
      CometActor(theSession, name, defaultXml, attributes) {
  def defaultPrefix = "ask_name"
    
  def render = ajaxForm(<div>What is your username?</div> ++ text("",name => answer(name.trim)) ++ 
    <input type="submit" value="Enter"/>)
}
