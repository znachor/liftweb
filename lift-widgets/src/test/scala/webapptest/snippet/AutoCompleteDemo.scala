package webapptest.snippet

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.widgets.autocomplete._

class AutoCompleteDemo {

  def render(xhtml: NodeSeq) :NodeSeq = {
    AutoComplete("", (current, limit) => {
      println("current = " + current)
      (1 to limit).map(v => "Value_" + v)
    }, s => println("submit " + s))
  }

}
