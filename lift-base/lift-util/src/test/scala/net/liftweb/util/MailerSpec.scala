package net.liftweb.util

import _root_.org.specs.runner._
import _root_.org.specs._
import common._


class MailerSpecTest extends Runner(MailerSpec) with JUnit
object MailerSpec extends Specification with Mailer {
  var lastMessage: Option[javax.mail.internet.MimeMessage] = None
  override val msgSender = {
    new MsgSender {
      override def sendIt(msg: javax.mail.internet.MimeMessage) = lastMessage = Some(msg)
    }
  }
  "Mailer" should {
    "deliver simple messages as simple messages" in {
      lastMessage = None
      sendMail(
        From("sender@nowhere.com"),
        Subject("This is a simple email"),
        To("recipient@nowhere.com"),
        PlainMailBodyType("Here is some plain text.")
      )
      while(lastMessage == None) Thread.sleep(100)
      lastMessage.get.isMimeType("text/plain") must_== true
      lastMessage.get.isMimeType("multipart/*") must_== false
    }
    "deliver multipart messages as multipart" in {
      lastMessage = None
      sendMail(
        From("sender@nowhere.com"),
        Subject("This is a multipart email"),
        To("recipient@nowhere.com"),
        PlainMailBodyType("Here is some plain text."),
        PlainMailBodyType("Here is some more plain text.")
      )
      while(lastMessage == None) Thread.sleep(100)
      lastMessage.get.isMimeType("text/plain") must_== false
      lastMessage.get.isMimeType("multipart/*") must_== true
    }
    "deliver rich messages as multipart" in {
      lastMessage = None
      sendMail(
        From("sender@nowhere.com"),
        Subject("This is a rich email"),
        To("recipient@nowhere.com"),
        XHTMLMailBodyType(<html><body>Here is some rich text</body></html>)
      )
      while(lastMessage == None) Thread.sleep(100)
      lastMessage.get.isMimeType("text/plain") must_== false
      lastMessage.get.isMimeType("multipart/*") must_== true
    }
  }
}
