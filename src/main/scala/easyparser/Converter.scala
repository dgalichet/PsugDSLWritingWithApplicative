package easyparser

import java.text.{ParseException, DateFormat}
import java.util.Date

/**
 * @author David Galichet.
 */
trait Converter[T] {
  def convert(p: String => Result[String]): Reader[T]
}

object Converter {
  implicit val string2StringConverter = new Converter[String] {
    override def convert(p: (String) => Result[String]) = Reader[String](p)
  }

  implicit val string2IntConverter = new Converter[Int] {
    override def convert(p: (String) => Result[String]) = Reader[Int] { s: String =>
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

  implicit def string2DateConverter(implicit dtFormat: DateFormat) = new Converter[Date] {
    override def convert(p: (String) => Result[String]) = Reader[Date] { s: String =>
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
