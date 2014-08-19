package easyparser

import scala.xml.{Elem, NodeSeq, XML}

/**
 * @author David Galichet.
 */
case class Picker(p: String => Result[String]) {

  def as[T](implicit c: Converter[T]): Reader[T] = c.convert(p)

}

object CsvPicker {
  def apply[T](i: Int)(implicit separator: Char): Picker = Picker { s: String =>
      val elems = s.trim.split(separator)
      if (i > 0 && elems.size > i) Success(elems(i).trim)
      else Failure(s"No column ${i} for ${s}")
  }
}

object XmlPicker {
  def apply[T](query: Elem => NodeSeq): Picker = Picker { s: String =>
    try {
      val xml = XML.loadString(s)
      Success(query(xml).text)
    } catch {
      case e: Exception => Failure(e.getMessage)
    }
  }
}