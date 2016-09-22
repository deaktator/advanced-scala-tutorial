package deaktator.advtut.futures.cats

import scala.language.higherKinds

trait Functor[C[_]] {
  def map[A, B](x: C[A])(f: A => B): C[B]
}

object Functor {
  def apply[F[_]](implicit f: Functor[F]) = f
}
