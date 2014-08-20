package easyparser

import org.specs2.mutable.Specification

/**
 * @author David Galichet.
 */
class ReaderTest extends Specification {
  import play.api.libs.functional.syntax._
  import Reader._

  "Reader and method" should {
    "combine two reader when results are success" in {
      val r1: Reader[String, String] = Reader { s => Success(s.toUpperCase) }
      val r2: Reader[String, String] = Reader { s => Success(s.capitalize) }
      val r = (r1 and r2)((_, _))
      r("test") === Success(("TEST", "Test"))
    }
    "return a Failure if one Failure is encountered" in {
      val r1: Reader[String, String] = Reader { s => Success(s.toUpperCase) }
      val r2: Reader[String, String] = Reader { s => Failure("Failed") }
      val r = (r1 and r2)((_, _))
      r("test") === Failure("Failed") and r("test") === Failure("Failed")
    }
    "return a Failure accumulating errors if two errors are encountered" in {
      val r1: Reader[String, String] = Reader { s => Failure("message 1") }
      val r2: Reader[String, String] = Reader { s => Failure("message 2") }
      val r = (r1 and r2)((_, _))
      r("test") === Failure(NEL("message 1", "message 2"))
    }
  }
  "Reader map method" should {
    "Return a failure if a failure is encountered" in {
      val r: Reader[String, String] = Reader { s => Failure("Error") }
      r.map(_.toUpperCase)("test") === Failure("Error")
    }
    "Return a transformed result if success" in {
      val r: Reader[String, String] = Reader { s => Success(s) }
      r.map(_.toUpperCase)("test") === Success("TEST")
    }
    "Have a reduce alias" in {
      val r1: Reader[String, String] = Reader { s => Success(s.capitalize) }
      val r2: Reader[String, String] = Reader { s => Success(s.capitalize) }
      (r1 and r2)(Person).apply("test") === Success(Person("Test", "Test"))
    }
  }
  "Reader flatMap" should {
    "Return a failure if entry failed" in {
      val r: Reader[String, String] = Reader { _ => Failure("Error") }
      r.flatMap(_ => Reader(_ => Success("OK")))("test") === Failure("Error")
    }
    "Return a failure if function do a failure" in {
      val r: Reader[String, String] = Reader { Success(_) }
      r.flatMap(_ => Reader[String, String](_ => Failure("Error")))("test") === Failure("Error")
    }
    "Return a success if both entry and functions returns a success" in {
      val r: Reader[String, String] = Reader { Success(_) }
      r.flatMap(_ => Reader[String, String]( s => Success(s.capitalize)) )("test") === Success("Test")
    }
  }
  "Reader verify" should {
    "Return a failure if entry Reader failed" in {
      val r: Reader[String, String] = Reader { s => Failure("Error") }
      r.verify(Success(_))("test") === Failure("Error")
    }
    "Return a failure if verify method failed" in {
      val r: Reader[String, String] = Reader { Success(_) }
      r.verify { x =>  if (x == "OK") Success(x) else Failure("KO") }("Bad entry") === Failure("KO")
    }
    "Return a success if verify method succeed" in {
      val r: Reader[String, String] = Reader { Success(_) }
      r.verify { x =>  if (x == "OK") Success(x) else Failure("KO") }("OK") === Success("OK")
    }
  }

  case class Person(firstname: String, lastname: String)
}

