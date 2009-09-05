package net.liftweb.json

/*
 * Copyright 2009 WorldWide Conferencing, LLC
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

import java.util.Date

trait Formats {
  val dateFormat: DateFormat
}

trait DateFormat {
  def parse(s: String): Option[Date]
}

object DefaultFormats extends DefaultFormats
trait DefaultFormats extends Formats {
  import java.text.{ParseException, SimpleDateFormat}

  val dateFormat = new DateFormat {
    def parse(s: String) = try {
      Some(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'").parse(s))
    } catch {
      case e: ParseException => None
    }
  }
}
