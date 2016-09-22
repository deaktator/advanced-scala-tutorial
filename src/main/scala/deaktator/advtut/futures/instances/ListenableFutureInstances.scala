package deaktator.advtut.futures.instances

import java.util.concurrent.Callable

import com.google.common.util.concurrent.{AsyncFunction, Futures, ListeningExecutorService, ListenableFuture}
import deaktator.advtut.futures.cats.Monad
import com.google.common.base.{Function => GFunction}
import deaktator.advtut.futures.instances.ListenableFutureInstances._

/**
  * Created by ryan on 9/21/16.
  */
case class ListenableFutureInstances(exSvc: ListeningExecutorService) extends Monad[ListenableFuture] {
  /**
    * Compute `asyncExecutedValue` asynchronously.
    * @param asyncExecutedValue what to execute asynschronously
    * @tparam A type of value to compute.
    * @return a future to containing the result of `asyncExecutedValue`.
    */
  override def point[A](asyncExecutedValue: => A): ListenableFuture[A] = {
    exSvc.submit(new Callable[A]{
      override def call(): A = asyncExecutedValue
    })
  }

  override def flatMap[A, B](ma: ListenableFuture[A])(f: A => ListenableFuture[B]): ListenableFuture[B] =
    Futures.transform(ma, GAsyncFn(f))

  override def map[A, B](fa: ListenableFuture[A])(f: A => B): ListenableFuture[B] =
    Futures.transform(fa, GFn(f))            // flatMap(fa)(a => point(f(a)))

  override def ap[A, B](x: ListenableFuture[A])(f: ListenableFuture[A => B]): ListenableFuture[B] =
    flatMap(f)(g => map(x)(y => g(y)))       // map2(x, f)((y, g) => g(y))

  override def map2[A, B, C](x: ListenableFuture[A], y: ListenableFuture[B])(f: (A, B) => C): ListenableFuture[C] =
    flatMap(y)(b => map(x)(a => f(a, b)))    // ap(y)(map(x)(f.curried))
}

object ListenableFutureInstances {
  object Implicits {
    implicit def listenableFutureInstances(implicit exSvc: ListeningExecutorService): ListenableFutureInstances =
      ListenableFutureInstances(exSvc)
  }

  case class GFn[A, B](f: A => B) extends GFunction[A, B] {
    def apply(a: A): B = f(a)
  }

  case class GAsyncFn[A, B](f: A => ListenableFuture[B]) extends AsyncFunction[A, B] {
    def apply(a: A): ListenableFuture[B] = f(a)
  }
}
