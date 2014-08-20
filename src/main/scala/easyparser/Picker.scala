package easyparser

import scala.xml.{Elem, NodeSeq}

/**
 * @author David Galichet.
 */
case class Picker[I](p: I => Result[String]) {

  def as[T](implicit c: Converter[I, T]): Reader[I, T] = c.convert(p)

}

object CsvPicker {
  def apply[T](i: Int): Picker[List[String]] = Picker { elems: List[String] =>
      if (i > 0 && elems.size > i) Success(elems(i).trim)
      else Failure(s"No column ${i} found in ${elems.mkString(";")}")
  }
}

object XmlPicker {
  def apply[T](query: Elem => NodeSeq): Picker[Elem] = Picker { elem: Elem =>
    try {
      Success(query(elem).text)
    } catch {
      case e: Exception => Failure(e.getMessage)
    }
  }
}