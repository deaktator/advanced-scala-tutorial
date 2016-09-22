package deaktator.advtut.futures.cats

import scala.language.higherKinds

trait Monad[C[_]] extends Applicative[C] {
  def flatMap[A, B](x: C[A])(f: A => C[B]): C[B]
}

object Monad {
  def apply[M[_]](implicit m: Monad[M]) = m
}
