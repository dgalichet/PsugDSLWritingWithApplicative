package easyparser

/**
 * @author David Galichet.
 */
sealed trait Result[+T]

case class Success[T](t: T) extends Result[T]

case class Failure(error: NonEmptyList[String]) extends Result[Nothing]

object Failure {
  def apply(s: String): Failure = Failure(NEL(s))
}
