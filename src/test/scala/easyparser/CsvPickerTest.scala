package easyparser

import java.text.SimpleDateFormat
import java.util.Date

import org.specs2.mutable.Specification

/**
 * @author David Galichet.
 */
class CsvPickerTest extends Specification {
  implicit val separator = ';'
  implicit val dtFormatter = new SimpleDateFormat("dd/MM/yyyy")
  import Converter.string2StringConverter
  import Converter.string2DateConverter

  import play.api.libs.functional.syntax._
  import Reader.readerIsAnApplicative

  "A CsvPicker" should {
    "be able to fetch a String in a CSV line" in {
      CsvPicker(1).as[String].apply("foo; bar") === Success("bar")
    }
    "be able to fetch a Date in a CSV line" in {
      CsvPicker(2).as[Date].apply("foo;bar;12/10/2014") === Success(dtFormatter.parse("12/10/2014"))
    }
    "return a Failure if index corresponds to a column that doesn't exists" in {
      CsvPicker(2).as[String].apply("foo;bar") === Failure("No column 2 for foo;bar")
    }
    "is unable to parse column as expected" in {
      CsvPicker(2).as[Date].apply("foo;bar;Wrong date") === Failure("Unable to format 'Wrong date' as Date")
    }
    "is able to combine two Pickers" in {
      val reader = (
        CsvPicker(1).as[String] and
        CsvPicker(2).as[Date]
      )(FutureEvent)
      reader("foo;bar;12/10/2014") === Success(FutureEvent("bar", dtFormatter.parse("12/10/2014")))
    }
  }
}

case class FutureEvent(name: String, dt: Date)
