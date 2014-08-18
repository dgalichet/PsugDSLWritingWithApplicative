package easyparser

/**
 * @author David Galichet.
 */
case class NonEmptyList[T](head: T, tail: List[T]) {
  def toList = head::tail

  def ++(l2: NonEmptyList[T]): NonEmptyList[T] = NonEmptyList(head, tail ++ l2.toList)
}

object NEL {
  def apply[T](h: T, t: T*) = NonEmptyList(h, t.toList)
}
