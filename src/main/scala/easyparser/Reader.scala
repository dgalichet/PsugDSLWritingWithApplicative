package easyparser

/**
 * @author David Galichet.
 */
case class Reader[O](p: String => Result[O]) {
  def apply(s: String): Result[O] = p(s)
}
