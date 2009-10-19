package net.liftweb.util
import _root_.org.joda.time.DateTime
import _root_.org.specs.Specification
import _root_.org.specs.runner._
import _root_.org.scalacheck.Gen._
import _root_.org.scalacheck.Prop._
import _root_.org.scalacheck.Arbitrary
import _root_.org.specs.Products._
import _root_.org.specs.mock.Mocker
import _root_.org.specs.ScalaCheck

import _root_.net.liftweb.util.TimeHelpers._
// import _root_.net.liftweb.util.JodaTimeHelpers._

class JodaTimeHelpersTest extends JUnit4(JodaTimeHelpersSpec)
object JodaTimeHelpersSpec extends Specification with TimeHelpers with TimeAmountsGen with ScalaCheck with Mocker with LoggerDelegation with ControlHelpers with ClassHelpers {
  def testEqualJodaSpan (first : TimeSpan, second : TimeSpan) = first must_== second

  "A JodaSpan" can {
    "be created from a number of milliseconds" in {
      TimeSpan(3000) must_== TimeSpan(3 * 1000)
    }
    "be created from a number of seconds" in {
      testEqualJodaSpan(3.seconds, TimeSpan(3 * 1000))
    }
    "be created from a number of minutes" in {
      testEqualJodaSpan(3.minutes, TimeSpan(3 * 60 * 1000))
    }
    "be created from a number of hours" in {
      testEqualJodaSpan(3.hours, TimeSpan(3 * 60 * 60 * 1000))
    }
    "be created from a number of days" in {
      testEqualJodaSpan(3.days, TimeSpan(3 * 24 * 60 * 60 * 1000))
    }
    "be created from a number of weeks" in {
      testEqualJodaSpan(3.weeks, TimeSpan(3 * 7 * 24 * 60 * 60 * 1000))
    }
    "be converted implicitly to a date starting from the epoch time" in {
      3.seconds.after(epoch) must beTrue
    }
    "be converted to a date starting from the epoch time, using the date method" in {
      3.seconds.date.after(epoch.toDate) must beTrue
    }
    "be implicitly converted to a Long" in {
      3.seconds must_== 3000L
    }
    "be compared to an int" in {
      3.seconds must_== 3000
      3.seconds must_!= 2000
    }
    "be compared to a long" in {
      3.seconds must_== 3000L
      3.seconds must_!= 2000L
    }
    "be compared to another JodaSpan" in {
      3.seconds must_== 3.seconds
      3.seconds must_!= 2.seconds
    }
    "be compared to another object" in {
      3.seconds must_!= "string"
    }
  }
  "A JodaSpan" should {
    "return a new JodaSpan representing the sum of the 2 times when added with another JodaSpan" in {
      3.seconds + 3.seconds must_== 6.seconds
    }
    "return a new JodaSpan representing the difference of the 2 times when substracted with another JodaSpan" in {
      3.seconds - 4.seconds must_== (-1).seconds
    }
    "have a later method returning a date relative to now plus the time span" in {
      3.seconds.later.millis must beCloseTo((jtNow + 3.seconds).millis, 100L)
    }
    "have an ago method returning a date relative to now minus the time span" in {
      3.seconds.ago.millis must beCloseTo((jtNow - 3.seconds).millis, 100L)
    }
    "have a toString method returning the relevant number of weeks, days, hours, minutes, seconds, millis" in {
      val conversionIsOk = forAll(timeAmounts)((t: TimeAmounts) => { val (timeSpanToString, timeSpanAmounts) = t
        timeSpanAmounts forall { case (amount, unit) =>
          amount >= 1  &&
          timeSpanToString.contains(amount.toString) || true }
      })
      val timeSpanStringIsPluralized = forAll(timeAmounts)((t: TimeAmounts) => { val (timeSpanToString, timeSpanAmounts) = t
        timeSpanAmounts forall { case (amount, unit) =>
               amount > 1  && timeSpanToString.contains(unit + "s") ||
               amount == 1 && timeSpanToString.contains(unit) ||
               amount == 0 && !timeSpanToString.contains(unit)
        }
      })
      conversionIsOk && timeSpanStringIsPluralized must pass
    }
  }
  "the JodaTimeHelpers" should {
    "provide a 'seconds' function transforming a number of seconds into millis" in {
      seconds(3) must_== 3 * 1000
    }
    "provide a 'minutes' function transforming a number of minutes into millis" in {
      minutes(3) must_== 3 * 60 * 1000
    }
    "provide a 'hours' function transforming a number of hours into milliss" in {
      hours(3) must_== 3 * 60 * 60 * 1000
    }
    "provide a 'days' function transforming a number of days into millis" in {
      days(3) must_== 3 * 24 * 60 * 60 * 1000
    }
    "provide a 'weeks' function transforming a number of weeks into millis" in {
      weeks(3) must_== 3 * 7 * 24 * 60 * 60 * 1000
    }
    "provide a noTime function on DateTime objects to transform a date into a date at the same day but at 00:00" in {
      TimeRules.formatTime(jtNow.noTime) must_== "00:00:00"
    }
    "provide a midnight function on DateTime objects to transform a date into a date at the same day but at 00:00" in {
      TimeRules.formatTime(jtNow.midnight) must_== "00:00:00"
    }
    "provide a day extension on DateTime returning the day of month corresponding to a given date" in {
      jtToday.withDay(3).day must_== 3
    }
    "provide an implicit conversion from DateTimeProperty to Int" in {
      (jtToday.withDay(1).day : Int) must_== 1
    }
    "provide an implicit conversion from DateTimeProperty to String" in {
      (jtToday.withYear(2008).year : String) must_== "2008"
    }
    "provide a month extension on DateTime returning the month corresponding to a given date" in {
      (jtToday.withMonth(4).month: Int) must_== 4
    }
    "provide a millisToDays function returning the number of days since the epoch time" in {
      millisToDays(epoch.millis) must_== 0
      millisToDays(jtToday.withYear(1970).withMonth(1).withDay(1).millis) must_== 0 // the epoch time
      // on the 3rd day after the epoch time, 2 days are passed
      millisToDays(jtToday.utc.withYear(1970).withMonth(1).withDay(3).millis) must_== 2
    }
    "provide a daysSinceEpoch function returning the number of days since the epoch time" in {
      daysSinceEpoch must_== millisToDays(jtNow.millis)
    }
    "provide a time function creating a new Date object from a number of millis" in {
      jtTime(1000L) must_== new DateTime(1000L)
    }
    "provide a calcTime function returning the time taken to evaluate a block in millis and the block's result" in {
      val (time, result) = calcTime((1 to 10).reduceLeft[Int](_ + _))
      time.toInt must beCloseTo(0, 1000)  // it should take less than 1 second!
      result must_== 55
    }
    "provide a logTime function logging the time taken to do something and returning the result" in {
      skip("this way of mock LiftLogger is not robust enough and has to be reworked")
      val logMock = new LiftLogger {
        override def info(a: => AnyRef) = record {
          a.toString must beMatching("this test took \\d* Milliseconds")
        }
      }
      expect {
        logMock.info("this test took 10 Milliseconds")
      }
      withLogger(logMock) {
        logTime("this test")((1 to 10).reduceLeft[Int](_ + _))
      }
    }

    "provide a formattedDateNow function to format todays date" in {
      formattedDateNow must beMatching("\\d\\d\\d\\d/\\d\\d/\\d\\d")
    }
    "provide a formattedTimeNow function to format now's time with the TimeZone" in {
      formattedTimeNow must beMatching("\\d\\d:\\d\\d ...(\\+|\\-\\d\\d:00)?")
    }

    "provide a parseInternetDate function to parse a string formatted using the internet format" in {
      TimeRules.parseInternetDate(TimeRules.formatInternetDate(jtNow)).open_!.getMillis must beCloseTo(jtNow.getMillis.toLong, 1000L)
    }
    "provide a parseInternetDate function returning new Date(0) if the input date cant be parsed" in {
      parseInternetDate("unparsable") must_== new java.util.Date(0)
    }
    "provide a toInternetDate function formatting a date to the internet format" in {
      toInternetDate(jtNow) must beMatching("..., \\d* ... \\d\\d\\d\\d \\d\\d:\\d\\d:\\d\\d .*")
    }
    "provide a toDate returning a Full(DateTime) from many kinds of objects" in {
      val d = jtNow
      (null, Nil, None, Failure("", Empty, Empty)).toList forall { toJTDate(_) must_== Empty }
      (Full(d), Some(d), d :: d).toList forall { toJTDate(_) must_== Full(d) }
      toJTDate(TimeRules.formatInternetDate(d)).open_!.millis must beCloseTo(d.millis, 1000L)
    }
  }

  "the TimeRules object" should {
    "provide a function to format the time of a DateTime object" in {
      TimeRules.formatTime(jtNow.midnight) must_== "00:00:00"
    }
    "provide a function to format the date of a DateTime object" in {
      TimeRules.formatDate(epoch) must_== "1970/01/01"
    }
    // TODO: More specs needed here
  }
}
