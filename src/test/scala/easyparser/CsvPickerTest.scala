package easyparser

import java.text.SimpleDateFormat
import java.util.Date

import org.specs2.mutable.Specification

/**
 * @author David Galichet.
 */
class CsvPickerTest extends Specification {
  implicit val dtFormatter = new SimpleDateFormat("dd/MM/yyyy")
  import Converter._

  import play.api.libs.functional.syntax._
  import Reader._

  "A CsvPicker" should {
    "be able to fetch a String in a CSV line" in {
      CsvPicker(1).as[String].apply(List("foo", "bar")) === Success("bar")
    }
    "be able to fetch a Date in a CSV line" in {
      CsvPicker(2).as[Date].apply(List("foo", "bar", "12/10/2014")) === Success(dtFormatter.parse("12/10/2014"))
    }
    "return a Failure if index corresponds to a column that doesn't exists" in {
      CsvPicker(2).as[String].apply(List("foo", "bar")) === Failure("No column 2 found in foo;bar")
    }
    "is unable to parse column as expected" in {
      CsvPicker(2).as[Date].apply(List("foo", "bar", "Wrong date")) === Failure("Unable to format 'Wrong date' as Date")
    }
    "is able to combine two Pickers" in {
      val reader = (
        CsvPicker(1).as[String] and
        CsvPicker(2).as[Date]
      )(FutureEvent)
      reader(List("foo", "bar", "12/10/2014")) === Success(FutureEvent("bar", dtFormatter.parse("12/10/2014")))
    }
    "is able to accumulate errors" in {
      val reader = (
        CsvPicker(1).as[Int] and
        CsvPicker(2).as[Date]
        )((_, _))
      reader(List("foo", "not a number", "not a date")) === Failure(NEL("Unable to format 'not a number' as Int", "Unable to format 'not a date' as Date"))
    }
  }
}

case class FutureEvent(name: String, dt: Date)
