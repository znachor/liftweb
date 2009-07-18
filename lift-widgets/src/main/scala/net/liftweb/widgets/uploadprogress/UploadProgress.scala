package net.liftweb.widgets.uploadprogress

import _root_.scala.xml.{NodeSeq,Text}
import _root_.net.liftweb.http.{SessionVar,Req,GetRequest,PlainTextResponse,JsonResponse,
                                LiftRules,OnDiskFileParamHolder,S,ResourceServer}
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.js.JE._
import _root_.net.liftweb.util.{Box,Empty,Failure,Full,Log}

import _root_.scala.actors.Actor
import _root_.scala.actors.Actor._

/**
 * A helper widget that makes it easy to do upload
 * progress bars using ajax polling.
 * 
 * @author Tim Perrett
 */
object UploadProgress {
  
  /**
   * Call UploadProgress.init from Boot.scala
   */ 
  def init = {
    /**
     * Enable file streaming uploads - this is required for progress updates
     */
    LiftRules.handleMimeFile = (fieldName, contentType, fileName, inputStream) =>
      OnDiskFileParamHolder(fieldName, contentType, fileName, inputStream)
    
    
    LiftRules.maxMimeSize = 1024 * 1024 * 1024
    LiftRules.maxMimeFileSize = LiftRules.maxMimeSize
    
    ResourceServer.allow({
      case "uploadprogress" :: "jquery.uploadprogress.0.3.js" :: Nil => true
      case "uploadprogress" :: "jquery.timers-1.1.2.js" :: Nil => true
    })
    
    LiftRules.dispatch.append {
      case Req("progress" :: Nil, "", GetRequest) => () => {
        val recived: Double = StatusHolder.is.map(_._1.toDouble).openOr(0D)
        val size: Double = StatusHolder.is.map(_._2.toDouble).openOr(0D)
        val state: String = if(recived == size){ "completed" } else { "uploading" }

        Log.info("DISPATCHING UPLOAD PROGRESS")
        println(StatusHolder.is)
        
        Full(JsonResponse(
          JsObj("state" -> state, 
                "percentage" -> Str(Math.floor((recived.toDouble / size.toDouble)*100).toString
          ))
        ))
      }
    }
    
  }
  
  def sessionProgessListener =
    S.session.foreach(s => { 
      s.progressListener = Full((pBytesRead: Long, pBytesTotal: Long, pItem: Int) => {
        StatusHolder(Full((pBytesRead, pBytesTotal)))
      })
    })
  
  def head(xhtml: NodeSeq): NodeSeq = {
    StatusHolder.is
    Script(Run(""" 
    $(function() {
      $('""" + S.attr("formId").openOr("form") + """').uploadProgress({
        start:function(){ },
        uploading: function(upload) {$('#percents').html(upload.percents+'%');},
        progressBar: '#""" + S.attr("progressBar").openOr("progressbar") + """',
        progressUrl: '""" + S.attr("progressUrl").openOr("/progress") + """',
        interval: """ + S.attr("interval").openOr("200") + """
      });
    });
    """))
  }
}

object StatusHolder extends SessionVar[Box[(Long, Long)]](Empty)