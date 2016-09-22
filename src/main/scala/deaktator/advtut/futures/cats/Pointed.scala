package deaktator.advtut.futures.cats

import scala.language.higherKinds

trait Pointed[P[_]] {
  def point[A](a: => A): P[A]
}

object Pointed {
  def apply[P[_]](implicit p: Pointed[P]): Pointed[P] = p
}
