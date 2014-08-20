package easyparser

import java.text.{ParseException, DateFormat}
import java.util.Date

/**
 * @author David Galichet.
 */
trait Converter[I, T] {
  def convert(p: I => Result[String]): Reader[I, T]
}

object Converter {
  implicit def stringConverter[I] = new Converter[I, String] {
    override def convert(p: I => Result[String]) = Reader[I, String](p)
  }

  implicit def intConverter[I] = new Converter[I, Int] {
    override def convert(p: I => Result[String]) = Reader[I, Int] { s: I =>
      p(s) match {
        case Success(t) => try {
          Success(t.toInt)
        } catch {
          case e: NumberFormatException => Failure(s"Unable to format '${t}' as Int")
        }
        case f: Failure => f
      }
    }
  }

  implicit def dateConverter[I](implicit dtFormat: DateFormat) = new Converter[I, Date] {
    override def convert(p: I => Result[String]) = Reader[I, Date] { s: I =>
      p(s) match {
        case Success(dt) => try {
          Success(dtFormat.parse(dt))
        } catch {
          case e: ParseException => Failure(s"Unable to format '${dt}' as Date")
        }
        case f: Failure => f
      }
    }
  }
}
