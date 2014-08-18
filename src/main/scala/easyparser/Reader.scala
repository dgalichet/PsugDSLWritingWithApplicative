package easyparser

/**
 * @author David Galichet.
 */
case class Reader[O](p: String => Result[O]) {
  def apply(s: String): Result[O] = p(s)

  def and[O2](r2: Reader[O2]): Reader[(O, O2)] = Reader { s: String =>
    (p(s), r2.p(s)) match {
      case (Success(s1), Success(s2)) => Success((s1, s2))
      case (Success(_), Failure(f)) => Failure(f)
      case (Failure(f), Success(_)) => Failure(f)
      case (Failure(f1), Failure(f2)) => Failure(f1 ++ f2)
    }
  }

  def map[T](f: O => T): Reader[T] = Reader { s: String =>
    p(s) match {
      case Success(o) => Success(f(o))
      case f: Failure => f
    }
  }
}
