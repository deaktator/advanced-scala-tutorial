package deaktator.advtut.futures.cats

import scala.language.higherKinds

trait Applicative[C[_]] extends Apply[C] with Pointed[C]

object Applicative {
  def apply[A[_]](implicit a: Applicative[A]): Applicative[A] = a
}
