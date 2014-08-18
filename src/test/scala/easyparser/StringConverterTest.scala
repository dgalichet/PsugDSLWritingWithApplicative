package easyparser

import java.text.SimpleDateFormat

import org.specs2.mutable.Specification

/**
 * @author David Galichet.
 */
class StringConverterTest extends Specification {

  "A String converter" should {
    "return String => Result[String] as is" in {
      val p: String => Result[String] = { s =>  Success(s) }
      val r = Converter.string2StringConverter.convert(p)
      r.p === p
    }
  }
  "a String to Int converter" should {
    "return a failure if we have a failure as input" in {
      val p: String => Result[String] = { s => Failure("Error") }
      val r = Converter.string2IntConverter.convert(p)
      r.p("toto") === p("toto")
    }
    "return a failure if unable to parse integer as expected" in {
      val p: String => Result[String] = { s => Success(s) }
      val r = Converter.string2IntConverter.convert(p)
      r.p("NaN") === Failure("Unable to format 'NaN' as Int")
    }
    "return a Success if we are able to parse integer" in {
      val p: String => Result[String] = { s => Success(s) }
      val r = Converter.string2IntConverter.convert(p)
      r.p("42") === Success(42)
    }
  }
  "a Date converter" should {
    implicit val dtFormat = new SimpleDateFormat("dd/MM/yyyy")
    "return a failure if we have a failure as input" in {
      val p: String => Result[String] = { s => Failure("Error") }
      val r = Converter.string2DateConverter.convert(p)
      r.p("toto") === p("toto")
    }
    "return a failure if unable to parse date as expected" in {
      val p: String => Result[String] = { s => Success(s) }
      val r = Converter.string2DateConverter.convert(p)
      r.p("Not a Date") === Failure("Unable to format 'Not a Date' as Date")
    }
    "return a Success if we are able to parse date" in {
      val p: String => Result[String] = { s => Success(s) }
      val r = Converter.string2DateConverter.convert(p)
      val testDate = "12/03/2014"
      r.p(testDate) === Success(dtFormat.parse(testDate))
    }
  }
}

