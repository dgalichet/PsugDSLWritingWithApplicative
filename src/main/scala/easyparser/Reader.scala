package easyparser

import play.api.libs.functional.{Functor, Applicative}

/**
 * @author David Galichet.
 */
case class Reader[O](p: String => Result[O]) {
  def apply(s: String): Result[O] = p(s)

//  def and[O2](r2: Reader[O2]): Reader[(O, O2)] = Reader { s: String =>
//    (p(s), r2.p(s)) match {
//      case (Success(s1), Success(s2)) => Success((s1, s2))
//      case (Success(_), Failure(f)) => Failure(f)
//      case (Failure(f), Success(_)) => Failure(f)
//      case (Failure(f1), Failure(f2)) => Failure(f1 ++ f2)
//    }
//  }

  def map[T](f: O => T): Reader[T] = Reader { s: String =>
    p(s) match {
      case Success(o) => Success(f(o))
      case f: Failure => f
    }
  }

  def reduce[T] = map[T] _ // alias for map
}

object Reader {

  def map2[O, O1, O2](r1: Reader[O1], r2: Reader[O2])(f: (O1, O2) => O): Reader[O] = Reader { s: String =>
    (r1.p(s), r2.p(s)) match {
      case (Success(s1), Success(s2)) => Success(f(s1, s2))
      case (Success(_), Failure(e)) => Failure(e)
      case (Failure(e), Success(_)) => Failure(e)
      case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
    }
  }

  implicit  val readerIsAFunctor: Functor[Reader] = new Functor[Reader] {
    override def fmap[A, B](m: Reader[A], f: (A) => B) = m.map(f)
  }

  implicit val readerIsAnApplicative: Applicative[Reader] = new Applicative[Reader] {
    override def pure[A](a: A) = Reader { _ => Success(a) }

    override def apply[A, B](mf: Reader[A => B], ma: Reader[A]) = map2(mf, ma)((f, a) => f(a))

    override def map[A, B](m: Reader[A], f: (A) => B) = m.map(f)
  }
}
