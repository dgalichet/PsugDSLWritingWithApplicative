package easyparser

/**
 * @author David Galichet.
 */
sealed trait Result[+T]

case class Success[T](t: T) extends Result[T]

case class Failure(error: String) extends Result[Nothing]
