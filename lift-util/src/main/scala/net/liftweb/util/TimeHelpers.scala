package net.liftweb.util

// These are deprecated imports and should eventually go away
import _root_.java.util.{Calendar,Date,Locale,TimeZone}
import _root_.java.text.SimpleDateFormat

// These are the new imports going forward
import _root_.java.text.DateFormat
import _root_.org.joda.time.{DateMidnight,DateTime,DateTimeZone,Duration,LocalTime,Period,ReadableDuration,ReadablePeriod}
import _root_.org.joda.time.format.{DateTimeFormat,DateTimeFormatter,PeriodFormatter,PeriodFormatterBuilder}

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * The TimeHelpers object extends the TimeHelpers. It can be imported to access all of the trait functions.
 */
object TimeHelpers extends TimeHelpers with ControlHelpers with ClassHelpers

/**
 * The TimeRules object defines global defaults for time formatting and parsing,
 * as well as variables than can be used to change the default behavior.
 */
object TimeRules {
  /** Formats a given DateTime as "HH:mm:ss" */
  val defaultTimeFormat = DateTimeFormat.forPattern("HH:mm:ss")

  /** Formats a given DateTime as "HH:mm zzz" */
  val defaultTimeWithZoneFormat = DateTimeFormat.forPattern("HH:mm zzz")

  /** Formats a given DateTime as "yyyy/MM/dd" */
  val defaultDateFormat = DateTimeFormat.forPattern("yyyy/MM/dd")

  /** Formats a given DateTime as "EEE, d MMM yyyy HH:mm:ss" */
  val defaultDateTimeFormat = DateTimeFormat.forPattern("EEE, d MMM yyyy HH:mm:ss")

  /** Formats a given DateTime as "EEE, d MMM yyyy HH:mm:ss z" in UTC. This has
   *  to be a java.text.DateFormat because Joda Time does not parse timezone names,
   *  only offsets */
  val internetDateFormat : DateFormat = {
    val ret = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss z", Locale.US)
    ret.setTimeZone(TimeHelpers.utc)
    ret
  }

  val defaultPeriodFormat =
  new PeriodFormatterBuilder()
    .appendMillis().appendSuffix(" millis", " millis").appendSeparator(", ", " and ")
    .appendSeconds().appendSuffix(" second", " seconds").appendSeparator(", ", " and ")
    .appendMinutes().appendSuffix(" minute", " minutes").appendSeparator(", ", " and ")
    .appendHours().appendSuffix(" hour", " hours").appendSeparator(", ", " and ")
    .appendDays().appendSuffix(" day", " days").appendSeparator(", ", " and ")
    .appendWeeks().appendSuffix(" week", " weeks").toFormatter


  /** This variable controls how time is formatted and parsed */
  var timeFormat = defaultTimeFormat

  /** This variable controls how time with a timezone is formatted and parsed */
  var timeWithZoneFormat = defaultTimeWithZoneFormat

  /** This variable controls how dates are parsed */
  var dateFormat = defaultDateFormat

  /** This variable controls how datetimes are parsed */
  var dateTimeFormat = defaultDateTimeFormat

  var periodFormat = defaultPeriodFormat

  type FormatFunc = DateTime => String

  /** This variable holds a function that can be used to format a time */
  var formatTime : FormatFunc = defaultFormatFunc(timeFormat, _)

  /** This variable holds a function that can be used to format a time with zone*/
  var formatTimeWithZone : FormatFunc = defaultFormatFunc(timeWithZoneFormat, _)

  /** This variable holds a function that can be used to format a date */
  var formatDate : FormatFunc = defaultFormatFunc(dateFormat, _)

  /** This variable holds a function that can be used to format a datetime */
  var formatDateTime : FormatFunc = defaultFormatFunc(dateTimeFormat, _)

  /** This variable holds a function that can be used to format a period */
  var formatPeriod : Period => String = defaultPeriodFormatFunc(periodFormat, _)

  /** Formats the given DateTime as a String using the internet date format */
  def formatInternetDate(in : DateTime) : String =
    in match {
      case null => formatInternetDate(new DateTime(0l))
      case s => internetDateFormat.format(in.toDate)
    }

  /** Formats the given Long as a String using the internet date format */
  def formatInternetDate(len : Long) : String =
    formatInternetDate(new DateTime(len))

  /** The default formatting handles null values as if they were the Epoch time */
  def defaultFormatFunc(formatter : DateTimeFormatter, in : DateTime) : String =
    in match {
      case null => defaultFormatFunc(formatter, TimeHelpers.epoch)
      case s => formatter.print(in)
    }

  /** The default period formatting handles null values as 0ms durations */
  def defaultPeriodFormatFunc(formatter : PeriodFormatter, in : Period) : String =
    in match {
      case null => defaultPeriodFormatFunc(formatter, new Period(0l))
      case s => formatter.print(in)
    }

  type ParseFunc = String => Box[DateTime]

  /** This variable holds a function that can be used to parse a time */
  var parseTime : ParseFunc = defaultParseFunc(timeFormat)

  /** This variable holds a function that can be used to parse a time with zone */
  var parseTimeWithZone : ParseFunc = defaultParseFunc(timeWithZoneFormat)

  /** This variable holds a function that can be used to parse a date */
  var parseDate : ParseFunc = defaultParseFunc(dateFormat)

  /** This variable holds a function that can be used to parse a datetime */
  var parseDateTime : ParseFunc = defaultParseFunc(dateTimeFormat)


  /** Parses the given String as an internet DateTime.
   *  @return Full(dateTime) or Failure if the String couldn't be parsed. */
  def parseInternetDate(dateString : String) : Box[DateTime] = {
    import ControlHelpers.tryo
    tryo {
      new DateTime(internetDateFormat.parse(dateString))
    }
  }

  def defaultParseFunc(parser : DateTimeFormatter)(in : String) : Box[DateTime] =
    try {
      Full(parser.parseDateTime(in))
    } catch {
      case e : IllegalArgumentException =>
        Failure("Could not parse " + in, Full(e), Empty)
    }
}

/**
 * This object defines implicit conversions between Java Date and Joda Time
 * DateTime instances.
 */
object JodaConversions {
  /** transforms a java.util.Date to a org.joda.time.DateTime */
  implicit def dateToDateTime(in : Date) : DateTime = new DateTime(in)

  /** transforms an org.joda.time.DateTime to a java.util.Date */
  implicit def dateTimeToDate(in : DateTime) : Date = in.toDate
}

/**
 * The TimeHelpers trait provide functions to create TimeSpans (an object representing an amount of time), to manage date formats
 * or general utility functions (get the date for today, get year/month/day number,...)
 */
trait TimeHelpers { self: ControlHelpers =>

  /** private variable allowing the access to all TimeHelpers functions from inside the TimeSpan class */
  private val outer = this

  /** transforms a long to a TimeSpanBuilder object. Usage: 3L.seconds returns a TimeSpan of 3000L millis  */
  implicit def longToTimeSpanBuilder(in: Long): TimeSpanBuilder = TimeSpanBuilder(in)

  /** transforms an int to a TimeSpanBuilder object. Usage: 3.seconds returns a TimeSpan of 3000L millis  */
  implicit def intToTimeSpanBuilder(in: Int): TimeSpanBuilder = TimeSpanBuilder(in)

  /** transforms a long to a TimeSpan object. Usage: 3000L returns a TimeSpan of 3000L millis  */
  implicit def longToTimeSpan(in: Long): TimeSpan = TimeSpan(in)

  /** transforms an int to a TimeSpan object. Usage: 3000 returns a TimeSpan of 3000L millis  */
  implicit def intToTimeSpan(in: Int): TimeSpan = TimeSpan(in)

  /** class building TimeSpans given an amount (len) and a method specify the time unit  */
  case class TimeSpanBuilder(val len: Long) {
    def seconds = TimeSpan(outer.seconds(len))
    def second = seconds
    def minutes = TimeSpan(outer.minutes(len))
    def minute = minutes
    def hours = TimeSpan(outer.hours(len))
    def hour = hours
    def days = TimeSpan(outer.days(len))
    def day = days
    def weeks = TimeSpan(outer.weeks(len))
    def week = weeks
  }

  /**
   * transforms a TimeSpan to a date by converting the TimeSpan expressed as millis and creating
   * a Date lasting that number of millis from the Epoch time (see the documentation for java.util.Date)
   */
  implicit def timeSpanToDate(in: TimeSpan): Date = in.date

  /** transforms a TimeSpan to its long value as millis */
  implicit def timeSpanToLong(in: TimeSpan): Long = in.millis

  /**
   * The TimeSpan class represents an amount of time.
   * It can be translated to a date with the date method. In that case, the number of millis seconds will be used to create a Date
   * object starting from the Epoch time (see the documentation for java.util.Date)
   */
  class TimeSpan(val millis: Long) {
    /** @return a Date as the amount of time represented by the TimeSpan after the Epoch date */
    def date = new Date(millis)

    /** @return a Date as the amount of time represented by the TimeSpan after now */
    def later = TimeSpan(millis + outer.millis).date

    /** @return a Date as the amount of time represented by the TimeSpan before now */
    def ago = TimeSpan(outer.millis - millis).date

  /*
      /** @return a DateTime representing our duration after the given DateTime */
    def after (instant : DateTime) : DateTime = instant.plus(duration)

    /** @return a DateTime representing our duration before the given DateTime */
    def before (instant : DateTime) : DateTime = instant.minus(duration)
*/

    /** @return true if this instant is after the other instant */
    def after (other : DateTime) = new DateTime(millis).isAfter(other)

    /** @return a TimeSpan representing the addition of 2 TimeSpans */
    def +(in: TimeSpan) = TimeSpan(this.millis + in.millis)

    /** @return a TimeSpan representing the substraction of 2 TimeSpans */
    def -(in: TimeSpan) = TimeSpan(this.millis - in.millis)

    /** override the equals method so that TimeSpans can be compared to long, int and TimeSpan */
    override def equals(cmp: Any) = {
      cmp match {
        case lo: Long => lo == this.millis
        case i: Int => i == this.millis
        case ti: TimeSpan => ti.millis == this.millis
        case _ => false
      }
    }

    /** override the toString method to display a readable amount of time */
    override def toString = TimeSpan.format(millis)

  def duration = new Duration(millis)
  }

  /**
   * The TimeSpan object provides class represents an amount of time.
   * It can be translated to a date with the date method. In that case, the number of millis seconds will be used to create a Date
   * object starting from the Epoch time (see the documentation for java.util.Date)
   */
  object TimeSpan {
    /** time units and values used when converting a total number of millis to those units (see the format function)  */
    val scales = List((1000L, "milli"), (60L, "second"), (60L, "minute"), (24L, "hour"), (7L, "day"), (10000L, "week"))

    /** explicit constructor for a TimeSpan  */
    def apply(in: Long) = new TimeSpan(in)

    /**
     * Formats a number of millis to a string representing the number of weeks, days, hours, minutes, seconds, millis
     */
    def format(millis: Long): String = {
      def divideInUnits(millis: Long) = scales.foldLeft[(Long, List[(Long, String)])]((millis, Nil)){ (total, div) =>
        (total._1 / div._1, (total._1 % div._1, div._2) :: total._2)
      }._2
      def formatAmount(amountUnit: (Long, String)) = amountUnit match {
        case (amount, unit) if (amount == 1) => amount + " " + unit
        case (amount, unit) => amount + " " + unit + "s"
      }
      divideInUnits(millis).filter(_._1 > 0).map(formatAmount(_)).mkString(", ")
    }
  }

  /** @return the current number of millis: System.currentTimeMillis  */
  def millis = System.currentTimeMillis

  /** @return the number of millis corresponding to 'in' seconds */
  def seconds(in: Long): Long = in * 1000L

  /** @return the number of millis corresponding to 'in' minutes */
  def minutes(in: Long): Long = seconds(in) * 60L

  /** @return the number of millis corresponding to 'in' hours */
  def hours(in: Long): Long = minutes(in) * 60L

  /** @return the number of millis corresponding to 'in' days */
  def days(in: Long): Long = hours(in) * 24L

  /** @return the number of millis corresponding to 'in' weeks */
  def weeks(in: Long): Long = days(in) * 7L

  /** implicit def used to add the noTime method to the Date class */
  implicit def toDateExtension(d: Date) = new DateExtension(d)

  /** This class adds a noTime method the Date class, in order to get at Date
   *  object starting at 00:00
      @deprecated Use JodaTime's DateTime.withTime(0,0,0,0) to do this instead */
  class DateExtension(date: Date) {
    /** @returns a Date object starting at 00:00 from date */
    def noTime = {
      val calendar = Calendar.getInstance
      calendar.setTime(date)
      calendar.set(Calendar.HOUR_OF_DAY, 0)
      calendar.set(Calendar.MINUTE, 0)
      calendar.set(Calendar.SECOND, 0)
      calendar.set(Calendar.MILLISECOND, 0)
      calendar.getTime
    }

    def millis = date.getTime
  }

  /** implicit def used to add the setXXX methods to the Calendar class
   * @deprecated Use the Joda Time "with" methods instead to handle this. */
  implicit def toCalendarExtension(c: Calendar) = new CalendarExtension(c)

  /** This class adds the setXXX methods to the Calendar class.
   *  Each setter returns the updated Calendar
   *  @deprecated Use the Joda Time "with" methods on DateTime instead to handle
   *  this. */
  class CalendarExtension(c: Calendar) {
    /** set the day of the month (1 based) and return the calendar */
    def setDay(d: Int) = { c.set(Calendar.DAY_OF_MONTH, d); c }

    /** set the month (0 based) and return the calendar */
    def setMonth(m: Int) = { c.set(Calendar.MONTH, m); c }

    /** set the year and return the calendar */
    def setYear(y: Int) = { c.set(Calendar.YEAR, y); c }

    /** set the TimeZone and return the calendar */
    def setTimezone(tz: TimeZone) = { c.setTimeZone(tz); c }

    /** set the time to 00:00:00.000 and return the calendar */
    def noTime = { c.setTime(c.getTime.noTime); c }
  }

  /** @return the date object for now */
  def now  = new Date

  /** @return the Calendar object for today (the TimeZone is the local TimeZone). Its time is 00:00:00.000 */
  def today  = Calendar.getInstance.noTime

  /** @return the current year */
  def currentYear: Int = new DateTime().year.get

  /**
   * @deprecated use now instead
   * @return the current time as a Date object
   */
  def timeNow = new Date


def jtNow = new DateTime()

  /**
   * @deprecated use today instead
   * @return the current Day as a Date object
   */
  def dayNow: Date = now.noTime

def jtDayNow = new ExtendedDateTime(jtNow).noTime

  /** alias for new Date(millis) */
  def time(when: Long) = new Date(when)

  /** @return the month corresponding to today (0 based, relative to UTC) */
  // Joda Time is 1 based on month, so we need to offset
  def month(in: Date): Int = new DateTime(in, DateTimeZone.UTC).monthOfYear.get - 1

  /** @return the year corresponding to today (relative to UTC) */
  def year(in: Date): Int =  new DateTime(in, DateTimeZone.UTC).year.get

  /** @return the day of month corresponding to the input date (1 based, relative to UTC) */
  def day(in: Date): Int =  new DateTime(in, DateTimeZone.UTC).dayOfMonth.get

  /** The UTC TimeZone */
  val utc = DateTimeZone.UTC.toTimeZone

  /** The UTC TimeZone */
  val jtUtc = DateTimeZone.UTC

  /** @return the number of days since epoch converted from millis */
  def millisToDays(millis: Long): Long = millis / (1000L * 60L * 60L * 24L)

  /** @return the number of days since epoch */
  def daysSinceEpoch: Long = millisToDays(millis)

  /** @return a DateTime representing the start of Epoch time (UTC). */
  def epoch = new DateTime(0L, DateTimeZone.UTC)

  /** @return the time taken to evaluate f in millis and the result */
  def calcTime[T](f: => T): (Long, T) = {
    val start = millis
    val result = f
    (millis - start, result)
  }

  /**
   * Log a message with the time taken in millis to do something and retrun the result
   * @return the result
   */
  def logTime[T](msg: String)(f: => T): T = {
    val (time, ret) = calcTime(f)
    Log.info(msg + " took " + time + " Milliseconds")
    ret
  }
    
  /**
   * @return a standard format HH:mm:ss
   * @deprecated use the TimeRules.timeFormat instead
   */
  val hourFormat = new SimpleDateFormat("HH:mm:ss")

  /**
   * @return the formatted time for a given Date
   * @deprecated use TimeRules.formatTime instead
   */
  def hourFormat(in: Date): String = hourFormat.format(in)

  /** @return a standard format for the date yyyy/MM/dd
      @deprecated use TimeRules.dateFormat instead */
  def dateFormatter = new SimpleDateFormat("yyyy/MM/dd")

  /** @return a format for the time which includes the TimeZone: HH:mm zzz
      @deprecated use TimeRules.timeWithZoneFormat instead */
  def timeFormatter = new SimpleDateFormat("HH:mm zzz")

  /** @return today's date formatted as yyyy/MM/dd
      @deprecated use TimeRules.formatDate instead*/
  def formattedDateNow = TimeRules.defaultDateFormat.print(jtNow)

  /** @return now's time formatted as HH:mm zzz
      @deprecated use TimeRules.formatTimeWithZone instead */
  def formattedTimeNow = TimeRules.defaultTimeWithZoneFormat.print(jtNow)

  /** @return a formatter for internet dates including: the day of week,
   *          the month, day of month, time and time zone
   *  @deprecated use TimeRules.internetDateFormat instead */
  def internetDateFormatter = {
    val ret = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss z", Locale.US)
    ret.setTimeZone(utc)
    ret
  }

  /** @return a date from a string using the internet format. Return the Epoch date if the parse is unsuccesfull */
  def boxParseInternetDate(dateString: String): Box[Date] = tryo {
    internetDateFormatter.parse(dateString)
  }

  /** @return a date from a string using the internet format. Return the Epoch
   *  date if the parse is unsuccesfull */
  def parseInternetDate(dateString: String): Date = 
    boxParseInternetDate(dateString) openOr new Date(0l)

  /** @return a date formatted with the internet format
   *  @deprecated use TimeRules.formatInternetDate instead */
  def toInternetDate(in: Date): String = TimeRules.formatInternetDate(new DateTime(in))


  def toInternetDate(in: DateTime): String = TimeRules.formatInternetDate(in)

  /** @return a date formatted with the internet format (from a number of millis)
   *  @deprecated use TimeRules.formatInternetDate instead */
  def toInternetDate(in: Long): String = TimeRules.formatInternetDate(in)

  /** @return the current time formatted as an internet date */
  def nowAsInternetDate: String = toInternetDate(millis)

  /** @return a Full(date) or a failure if the input couldn't be translated to date (or Empty if the input is null)*/
  def toDate(in: Any): Box[Date] = {
    try {
      in match {
        case null => Empty
        case d: Date => Full(d)
        case lng: Long => Full(new Date(lng))
        case lng: Number => Full(new Date(lng.longValue))
        case Nil | Empty | None | Failure(_, _, _) => Empty
        case Full(v) => toDate(v)
        case Some(v) => toDate(v)
        case v :: vs => toDate(v)
        case s : String => tryo(internetDateFormatter.parse(s)) or tryo(dateFormatter.parse(s))
        case o => toDate(o.toString)
      }
    } catch {
      case e => Log.debug("Error parsing date "+in, e); Failure("Bad date: "+in, Full(e), Empty)
    }
  }

  // TODO: This will go away when we refactor TimeSpan
  // implicit def timeSpanToJodaSpan(in: TimeSpan) = new JodaSpan(new Duration(in.millis))

  // This lets you use the DSL for JodaTime API constructs, like now plus (3 seconds)
  implicit def jodaSpanToDuration(in : TimeSpan) : Duration = new Duration(in.millis)
  implicit def durationToJodaSpan(in : Duration) : TimeSpan = new TimeSpan(in.getMillis)
  implicit def periodToJodaSpan(in : Period) : TimeSpan = new TimeSpan(in.toStandardDuration.getMillis)

/*
  class JodaSpan (val duration : Duration) {
    /** @return a new DateTime representing the current duration after Epoch time. */
    def date : DateTime = epoch.plus(duration)

    /** @return a DateTime as the amount of time represented by the JodaSpan after now */
    def later : DateTime = now.plus(duration)

    /** @return a DateTime as the amount of time represented by the TimeSpan before now */
    def ago : DateTime = now.minus(duration)

    /** @return a DateTime representing our duration after the given DateTime */
    def after (instant : DateTime) : DateTime = instant.plus(duration)

    /** @return a DateTime representing our duration before the given DateTime */
    def before (instant : DateTime) : DateTime = instant.minus(duration)

    /** @return a JodaSpan representing the addition of 2 TimeSpans */
    def +(in: JodaSpan) = new JodaSpan(duration.plus(in.duration))

    /** An alias for "+" */
    def and (in : JodaSpan) = this + in

    /** @return a JodaSpan representing the substraction of 2 TimeSpans */
    def -(in: JodaSpan) = new JodaSpan(duration.minus(in.duration))

    /** override the equals method so that TimeSpans can be compared to long, int and TimeSpan */
    override def equals(cmp: Any) = {
      cmp match {
        case lo: Long => lo == this.duration.getMillis
        case i: Int => i == this.duration.getMillis
        case ts: TimeHelpers#TimeSpan => ts.millis == this.duration.getMillis
        case ti: JodaSpan => ti.duration == this.duration
        case du: Duration => du == this.duration
        case _ => false
      }
    }

    /** override the toString method to display a readable amount of time */
    override def toString = TimeRules.formatPeriod(duration.toPeriod)
  }

  object JodaSpan {
    def apply(in : Long) = new JodaSpan(new Duration(in))
  }
*/
  /* ========================================================================
   * This section defines some convenience methods to extend DateTime
   * ======================================================================== */
  implicit def dateTimeToExtendedDateTime(in : DateTime) = new ExtendedDateTime(in)

  implicit def dateTimePropertyToInt(in : DateTime.Property) : Int = in.get

  implicit def dateTimePropertyToString(in : DateTime.Property) : String = in.getAsString

  /** This class enhances a DateTime with some more scala-ish syntax for things */
  class ExtendedDateTime(date : DateTime) {
    /** @return the date with time fields (h:m:s.ms) set to zero */
    def midnight : DateTime = date.withFields(LocalTime.MIDNIGHT)

    /** An alias for midnight */
    def noTime : DateTime = midnight

    /** @return an ExtendedDateTime representing the date in the UTC zone */
    def utc = date.withZone(TimeHelpers.jtUtc)

    /** Alias for dayOfMonth (to be relatively similar to TimeHelpers' usage) */
    def day : Int = date.dayOfMonth

    /** Alias for withDayOfMonth */
    def withDay (day : Int) = date.withDayOfMonth(day)

    /** @return the hour of the day, 0 based */
    def hour = date.hourOfDay

    /** Alias for DateTime.getMillis. Returns the number of millis since epoch, UTC. */
    def millis = date.getMillis

    /** Alias for DateTime.minuteOfHour */
    def minute = date.minuteOfHour

    /** @return the month corresponding to today (1 based, relative to local zone) */
    def month = date.monthOfYear

    /** Alias for withMonthOfYear */
    def withMonth (month : Int) = date.withMonthOfYear(month)

    /** @return the week of the year */
    def week = date.weekOfWeekyear

    /** Takes another DateTime and computes the duration between them. This method
        does not validate that the arguments are ordered chronologically, so you
        may get a negative value. */
    def to (end : DateTime) : Duration = new Duration(date, end)

    /** @return a new DateTime representing this DateTime plus the given time span */
    def + (span : TimeSpan) : DateTime = date.plus(span.duration)

    /** @return a new DateTime representing this DateTime plus the given time span */
    def + (span : ReadableDuration) : DateTime = date.plus(span)

    /** @return a new DateTime representing this DateTime plus the given time span */
    def + (span : ReadablePeriod) : DateTime = date.plus(span)

    /** @return a new DateTime representing this DateTime minus the given time span */
    def - (span : TimeSpan) : DateTime = date.minus(span.duration)

    /** @return a new DateTime representing this DateTime minus the given time span */
    def - (span : ReadableDuration) : DateTime = date.minus(span)

    /** @return a new DateTime representing this DateTime minus the given time span */
    def - (span : ReadablePeriod) : DateTime = date.minus(span)
  }

  /* ========================================================================
   * This section defines some convenience methods for points in time
   * ======================================================================== */
  /** @return the DateTime object for now */
  // def now : DateTime = new DateTime

  /** @return the DateTime object for today (the TimeZone is the local TimeZone).
   *  Its time is 00:00:00.000 */
  def jtToday : DateTime = jtNow.withFields(LocalTime.MIDNIGHT)

  /** @return a DateTime representing the start of Epoch time (UTC). */
  // def epoch = new DateTime(0, DateTimeZone.UTC)

  /* ========================================================================
   * This section defines some convenience methods for field extraction
   * ======================================================================== */
  /** alias for new Date(millis) */
  def jtTime(when: Long) = new DateTime(when)

  /** @return the month corresponding to today (0 based, relative to UTC)
   *  @deprecated The ExtendedDateTime wrapper providers direct accessors on a DateTime object */
  // Joda Time is 1 based on month, so we need to offset
  def month(in: DateTime): Int = in.withZone(jtUtc).monthOfYear.get

  /** @return the year corresponding to today (relative to UTC)
      @deprecated The ExtendedDateTime wrapper providers direct accessors on a DateTime object*/
  def year(in: DateTime): Int =  in.withZone(jtUtc).year.get

  /** @return the day of month corresponding to the input date (1 based, relative to UTC)
   *  @deprecated The ExtendedDateTime wrapper providers direct accessors on a DateTime object */
  def day(in: DateTime): Int =  in.withZone(jtUtc).dayOfMonth.get

  /** @return a Full(DateTime) or a failure if the input couldn't be
   *  translated to date (or Empty if the input is null)*/
  def toJTDate(in: Any): Box[DateTime] = {
    try {
      in match {
        case null => Empty
        case d: Date => Full(new DateTime(d))
        case d: DateTime => Full(d)
        case lng: Long => Full(new DateTime(lng))
        case lng: Number => Full(new DateTime(lng.longValue))
        case Nil | Empty | None | Failure(_, _, _) => Empty
        case Full(v) => toJTDate(v)
        case Some(v) => toJTDate(v)
        case v :: vs => toJTDate(v)
        case s : String => TimeRules.parseInternetDate(s) orElse TimeRules.parseDate(s)
        case o => toJTDate(o.toString)
      }
    } catch {
      case e => Log.debug("Error parsing date "+in, e); Failure("Bad date: "+in, Full(e), Empty)
    }
  }
}

/*
object JodaTimeHelpers extends JodaTimeHelpers {

}

/**
 * This trait is being used in the transition from java.util Date/Time handling
 * over to Joda Time. 
 */
trait JodaTimeHelpers {
  /* ========================================================================
   * This section defines JodaSpanBuilders and JodaSpans. These correspond
   * roughly to TimeHelpers' TimeSpanBuilders and TimeSpans, but we want to
   * use a different name to avoid conflicts. 
   * ======================================================================== */

  /* TODO: This will replace TimeSpan when the time comes to remove the old impl
  implicit def longToJodaSpanBuilder(in : Long) = JodaSpanBuilder(in)
  implicit def intToJodaSpanBuilder(in : Int) = longToJodaSpanBuilder(in)

  case class JodaSpanBuilder(val len : Long) {
    def seconds = new JodaSpan(Duration.standardSeconds(len))
    def second = seconds
    def minutes = new JodaSpan(Duration.standardMinutes(len))
    def minute = minutes
    def hours = new JodaSpan(Duration.standardHours(len))
    def hour = hours
    def days = new JodaSpan(Duration.standardDays(len))
    def day = days
    def weeks = new JodaSpan(Duration.standardDays(len * 7l))
    def week = weeks
  }
  */

  // TODO: This will go away when we refactor TimeSpan
  implicit def timeSpanToJodaSpan(in : TimeHelpers#TimeSpan) = new JodaSpan(new Duration(in.millis))
  
  // This lets you use the DSL for JodaTime API constructs, like now plus (3 seconds)
  implicit def jodaSpanToDuration(in : JodaSpan) : Duration = in.duration
  implicit def durationToJodaSpan(in : Duration) : JodaSpan = new JodaSpan(in)
  implicit def periodToJodaSpan(in : Period) : JodaSpan = new JodaSpan(in.toStandardDuration)

  class JodaSpan (val duration : Duration) {
    /** @return a new DateTime representing the current duration after Epoch time. */
    def date : DateTime = epoch.plus(duration)

    /** @return a DateTime as the amount of time represented by the JodaSpan after now */
    def later : DateTime = now.plus(duration)

    /** @return a DateTime as the amount of time represented by the TimeSpan before now */
    def ago : DateTime = now.minus(duration)

    /** @return a DateTime representing our duration after the given DateTime */
    def after (instant : DateTime) : DateTime = instant.plus(duration)

    /** @return a DateTime representing our duration before the given DateTime */
    def before (instant : DateTime) : DateTime = instant.minus(duration)

    /** @return a JodaSpan representing the addition of 2 TimeSpans */
    def +(in: JodaSpan) = new JodaSpan(duration.plus(in.duration))

    /** An alias for "+" */
    def and (in : JodaSpan) = this + in

    /** @return a JodaSpan representing the substraction of 2 TimeSpans */
    def -(in: JodaSpan) = new JodaSpan(duration.minus(in.duration))

    /** override the equals method so that TimeSpans can be compared to long, int and TimeSpan */
    override def equals(cmp: Any) = {
      cmp match {
        case lo: Long => lo == this.duration.getMillis
        case i: Int => i == this.duration.getMillis
        case ts: TimeHelpers#TimeSpan => ts.millis == this.duration.getMillis
        case ti: JodaSpan => ti.duration == this.duration
        case du: Duration => du == this.duration
        case _ => false
      }
    }

    /** override the toString method to display a readable amount of time */
    override def toString = TimeRules.formatPeriod(duration.toPeriod)
  }

  object JodaSpan {
    def apply(in : Long) = new JodaSpan(new Duration(in))
  }

  /* ========================================================================
   * This section defines some convenience methods to extend DateTime
   * ======================================================================== */
  implicit def dateTimeToExtendedDateTime(in : DateTime) = new ExtendedDateTime(in)

  implicit def dateTimePropertyToInt(in : DateTime.Property) : Int = in.get

  implicit def dateTimePropertyToString(in : DateTime.Property) : String = in.getAsString

  /** This class enhances a DateTime with some more scala-ish syntax for things */
  class ExtendedDateTime(date : DateTime) {
    /** @return the date with time fields (h:m:s.ms) set to zero */
    def midnight : DateTime = date.withFields(LocalTime.MIDNIGHT)

    /** An alias for midnight */
    def noTime : DateTime = midnight

    /** @return an ExtendedDateTime representing the date in the UTC zone */
    def utc = date.withZone(JodaTimeHelpers.utc)
  
    /** Alias for dayOfMonth (to be relatively similar to TimeHelpers' usage) */
    def day : Int = date.dayOfMonth

    /** Alias for withDayOfMonth */
    def withDay (day : Int) = date.withDayOfMonth(day)

    /** @return the hour of the day, 0 based */
    def hour = date.hourOfDay

    /** Alias for DateTime.getMillis. Returns the number of millis since epoch, UTC. */
    def millis = date.getMillis

    /** Alias for DateTime.minuteOfHour */
    def minute = date.minuteOfHour

    /** @return the month corresponding to today (1 based, relative to local zone) */
    def month = date.monthOfYear

    /** Alias for withMonthOfYear */
    def withMonth (month : Int) = date.withMonthOfYear(month)

    /** @return the week of the year */
    def week = date.weekOfWeekyear

    /** Takes another DateTime and computes the duration between them. This method
        does not validate that the arguments are ordered chronologically, so you
        may get a negative value. */
    def to (end : DateTime) : Duration = new Duration(date, end)

    /** @return a new DateTime representing this DateTime plus the given time span */
    def + (span : JodaSpan) : DateTime = date.plus(span.duration)

    /** @return a new DateTime representing this DateTime plus the given time span */
    def + (span : ReadableDuration) : DateTime = date.plus(span)

    /** @return a new DateTime representing this DateTime plus the given time span */
    def + (span : ReadablePeriod) : DateTime = date.plus(span)

    /** @return a new DateTime representing this DateTime minus the given time span */
    def - (span : JodaSpan) : DateTime = date.minus(span.duration)

    /** @return a new DateTime representing this DateTime minus the given time span */
    def - (span : ReadableDuration) : DateTime = date.minus(span)

    /** @return a new DateTime representing this DateTime minus the given time span */
    def - (span : ReadablePeriod) : DateTime = date.minus(span)
  }

  /* ========================================================================
   * This section defines some convenience methods for points in time
   * ======================================================================== */
  /** @return the DateTime object for now */
  def now : DateTime = new DateTime

  /** @return the DateTime object for today (the TimeZone is the local TimeZone).
   *  Its time is 00:00:00.000 */
  def today : DateTime = now.withFields(LocalTime.MIDNIGHT)

  /** @return a DateTime representing the start of Epoch time (UTC). */
  def epoch = new DateTime(0, DateTimeZone.UTC)

  /* ========================================================================
   * This section defines some convenience methods for field extraction
   * ======================================================================== */
  /** alias for new Date(millis) */
  def time(when: Long) = new DateTime(when)

  /** @return the month corresponding to today (0 based, relative to UTC)
   *  @deprecated The ExtendedDateTime wrapper providers direct accessors on a DateTime object */
  // Joda Time is 1 based on month, so we need to offset
  def month(in: DateTime): Int = in.withZone(utc).monthOfYear.get - 1

  /** @return the year corresponding to today (relative to UTC)
      @deprecated The ExtendedDateTime wrapper providers direct accessors on a DateTime object*/
  def year(in: DateTime): Int =  in.withZone(utc).year.get

  /** @return the day of month corresponding to the input date (1 based, relative to UTC)
   *  @deprecated The ExtendedDateTime wrapper providers direct accessors on a DateTime object */
  def day(in: DateTime): Int =  in.withZone(utc).dayOfMonth.get

  /** The UTC TimeZone. Just an alias for DateTimeZone.UTC */
  val utc = DateTimeZone.UTC

  /** @return a Full(DateTime) or a failure if the input couldn't be
   *  translated to date (or Empty if the input is null)*/
  def toDate(in: Any): Box[DateTime] = {
    try {
      in match {
        case null => Empty
        case d: Date => Full(new DateTime(d))
        case d: DateTime => Full(d)
        case lng: Long => Full(new DateTime(lng))
        case lng: Number => Full(new DateTime(lng.longValue))
        case Nil | Empty | None | Failure(_, _, _) => Empty
        case Full(v) => toDate(v)
        case Some(v) => toDate(v)
        case v :: vs => toDate(v)
        case s : String => TimeRules.parseInternetDate(s) orElse TimeRules.parseDate(s)
        case o => toDate(o.toString)
      }
    } catch {
      case e => Log.debug("Error parsing date "+in, e); Failure("Bad date: "+in, Full(e), Empty)
    }
  }
}
*/