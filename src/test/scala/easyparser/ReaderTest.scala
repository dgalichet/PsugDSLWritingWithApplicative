package easyparser

import org.specs2.mutable.Specification

/**
 * @author David Galichet.
 */
class ReaderTest extends Specification {
  import play.api.libs.functional.syntax._
  import Reader.readerIsAnApplicative

  "Reader and method" should {
    "combine two reader when results are success" in {
      val r1: Reader[String] = Reader { s => Success(s.toUpperCase) }
      val r2: Reader[String] = Reader { s => Success(s.capitalize) }
      val r = (r1 and r2).tupled
      r("test") === Success(("TEST", "Test"))
    }
    "return a Failure if one Failure is encountered" in {
      val r1: Reader[String] = Reader { s => Success(s.toUpperCase) }
      val r2: Reader[String] = Reader { s => Failure("Failed") }
      val r = (r1 and r2).tupled
      r("test") === Failure("Failed") and r("test") === Failure("Failed")
    }
    "return a Failure accumulating errors if two errors are encountered" in {
      val r1: Reader[String] = Reader { s => Failure("message 1") }
      val r2: Reader[String] = Reader { s => Failure("message 2") }
      val r = (r1 and r2).tupled
      r("test") === Failure(NEL("message 1", "message 2"))
    }
  }
  "Reader map method" should {
    "Return a failure if a failure is encountered" in {
      val r: Reader[String] = Reader { s => Failure("Error") }
      r.map(_.toUpperCase)("test") === Failure("Error")
    }
    "Return a transformed result if success" in {
      val r: Reader[String] = Reader { s => Success(s) }
      r.map(_.toUpperCase)("test") === Success("TEST")
    }
    "Have a reduce alias" in {
      val r1: Reader[String] = Reader { s => Success(s.capitalize) }
      val r2: Reader[String] = Reader { s => Success(s.capitalize) }
      (r1 and r2)(Person).apply("test") === Success(Person("Test", "Test"))
    }
  }

  case class Person(firstname: String, lastname: String)
}

