package deaktator.advtut.futures

import scala.language.higherKinds

package object cats {
  implicit class MonadOps[M[_], A](m: M[A])(implicit M: Monad[M]) {
    def map[B](f: A => B) = M.map(m)(f)
    def flatMap[B](f: A => M[B]) = M.flatMap(m)(f)
  }
}
