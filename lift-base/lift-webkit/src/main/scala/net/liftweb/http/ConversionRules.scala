/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.http


import net.liftweb.util.Helpers.{toInternetDate, tryo, dateFormatter=>dateFormat, hourFormat}
import net.liftweb.common._
import java.util.Date

object ConversionRules extends Factory {
  //val lazySnippetTimeout: FactoryMaker[TimeSpan] = new FactoryMaker(() => 30 seconds) {}
  private def nonnull(d: Date) = d match {
    case null => new Date(0L)
    case _ => d
  }
  
  /**
   * A function to format a Date as a date and time ... can be replaced by a function that is user-specific
   */
  val formatDateTime = new FactoryMaker[Date=>String]( () => (d: Date) => toInternetDate(nonnull(d)) ) {}
  
  /**
   * A function to format a Date as a date only
  */
  val formateDate = new FactoryMaker[Date=>String]( () => (d: Date) => dateFormat.format(nonnull(d)) ) {}
  
  /**
   * A function to format a Date as a time. By default uses Helpers.hourFormat which includes seconds but not time zone
  */
  val formatTime = new FactoryMaker[Date=>String]( () => (d: Date) => hourFormat.format(nonnull(d) ) {}
  @volatile var formatTime: Date => String = date => date match {
    case null => ""
    case _ => Helpers.hourFormat.format(date)
  }

  /**
   * A function that parses a String representing a date and time into a Date... can be replaced by something that's user-specific
   * By default uses Helpers.toDate which parses either as an "internet date" (date and time) or just a date if that fails.
   */
  @volatile var parseDateTime: String => Box[Date] = str => str match {
    case null => Empty
    case s => Helpers.toDate(s)
  }
  /**
   * A function that parses a String representing a date into a Date.
   * Defaults to parseDateTime for backward comaptibility
   */
  @volatile var parseDate: String => Box[Date] = parseDateTime
  /**
   * A function that parses a String representing a time into a Date.
   */
  @volatile var parseTime: String => Box[Date] = s => s match {
    case null => Empty
    case s => tryo{Helpers.hourFormat.parse(s)} or tryo {Helpers.timeFormatter.parse(s)}
  }
}
