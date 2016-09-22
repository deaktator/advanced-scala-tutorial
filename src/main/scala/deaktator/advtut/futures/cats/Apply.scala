package deaktator.advtut.futures.cats

import scala.language.higherKinds

trait Apply[C[_]] extends Functor[C] {
  def map2[A, B, D](x: C[A], y: C[B])(f: (A, B) => D): C[D]
  def ap[A, B](x: C[A])(f: C[A => B]): C[B]
}

object Apply {
  def apply[A[_]](implicit a: Apply[A]): Apply[A] = a
}