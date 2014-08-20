package easyparser

import play.api.libs.functional.{FunctionalCanBuild, FunctionalBuilderOps, Functor, Applicative}
import play.api.libs.functional.syntax._

/**
 * @author David Galichet.
 * inspired by @sadache https://gist.github.com/sadache/3646092.
 */
case class Reader[I, O](p: I => Result[O]) {
  def apply(s: I): Result[O] = p(s)

  def map[T](f: O => T): Reader[I, T] = Reader { s: I =>
    p(s) match {
      case Success(o) => Success(f(o))
      case f: Failure => f
    }
  }

  def flatMap[T](f: O => Reader[I, T]): Reader[I, T] = Reader { s: I =>
    p(s) match {
      case Success(o) => f(o)(s)
      case f: Failure => f
    }
  }

  def verify(f: O => Result[O]): Reader[I, O] = flatMap { o: O => Reader( _ => f(o)) }

}

object Reader {

  def map2[I, O, O1, O2](r1: Reader[I, O1], r2: Reader[I, O2])(f: (O1, O2) => O): Reader[I, O] = Reader { s: I =>
    (r1.p(s), r2.p(s)) match {
      case (Success(s1), Success(s2)) => Success(f(s1, s2))
      case (Success(_), Failure(e)) => Failure(e)
      case (Failure(e), Success(_)) => Failure(e)
      case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
    }
  }

  implicit  def readerIsAFunctor[I]: Functor[({type λ[A] = Reader[I, A]})#λ] = new Functor[({type λ[A] = Reader[I, A]})#λ] {
    override def fmap[A, B](m: Reader[I, A], f: (A) => B) = m.map(f)
  }

  implicit def readerIsAnApplicative[I]: Applicative[({type λ[A] = Reader[I, A]})#λ] = new Applicative[({type λ[A] = Reader[I, A]})#λ] {
    override def pure[A](a: A) = Reader { _ => Success(a) }

    override def apply[A, B](mf: Reader[I, A => B], ma: Reader[I, A]) = map2(mf, ma)((f, a) => f(a))

    override def map[A, B](m: Reader[I, A], f: (A) => B) = m.map(f)
  }

  import scala.language.implicitConversions

  // Here we help the compiler a bit. Thanks @skaalf (Julien Tournay) and https://github.com/jto/validation for the trick
  implicit def fcbReads[I]: FunctionalCanBuild[({type λ[A] = Reader[I, A]})#λ] = functionalCanBuildApplicative[({type λ[A] = Reader[I, A]})#λ]

  implicit def fboReads[I, A](a: Reader[I, A])(implicit fcb: FunctionalCanBuild[({type λ[x] = Reader[I, x]})#λ]) = new FunctionalBuilderOps[({type λ[x] = Reader[I, x]})#λ, A](a)(fcb)

}
