/*
 * Copyright (c) 2007, Paycorp Pty Limited. All Rights Reserved.
 *
 */
package bootstrap.liftweb

import java.util.Locale

import javax.servlet.http.HttpServletRequest

import net.liftweb.util.{Log,Empty,Full,LoanWrapper,LogBoot}
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import S.?

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Formatter;
import java.util.GregorianCalendar;
import java.util.Locale;

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
    def boot {
        LogBoot.defaultProps =
        """<?xml version="1.0" encoding="UTF-8" ?>
      <!DOCTYPE log4j:configuration SYSTEM "log4j.dtd">
      <log4j:configuration xmlns:log4j="http://jakarta.apache.org/log4j/">
      <appender name="appender" class="org.apache.log4j.ConsoleAppender">
      <layout class="org.apache.log4j.SimpleLayout"/>
      </appender>
      <root>
      <priority value ="DEBUG"/>
      <appender-ref ref="appender"/>
      </root>
      </log4j:configuration>
      """

        LiftRules.enableLiftGC = false;

        // where to search snippet
        LiftRules.addToPackages("au.com.immu")

        // Set up a site map
        val entries = SiteMap(Menu(Loc("Home", "index" :: Nil , ?("Home"))),
                              Menu(Loc("favourite", "immu" :: "favourite" :: Nil, "favourite")),
                              Menu(Loc("favouriteResp", "immu" :: "favouriteResp" :: Nil, "favouriteResp")),
                              Menu(Loc("favouriteHowWorks", "immu" :: "favouriteHowWorks" :: Nil, "favouriteHowWorks")),
                              Menu(Loc("error", "immu" ::  "error" :: Nil, "error"))
)

        LiftRules.setSiteMap(entries)

        // Output a simple error to the user. Link the error message to the screen via the current datetime
        LiftRules.exceptionHandler.prepend {
            case (mode, state, ex) =>
                val outputFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssz")
                val curDate = new Date()
                val dateStr = outputFormat.format(curDate);
                Log.error(dateStr, ex);
                RedirectResponse("/immu/error?"+dateStr);
        }


    }
}
