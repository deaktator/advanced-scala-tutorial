package deaktator.advtut.futures

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit.{MILLISECONDS, SECONDS}

import com.google.common.util.concurrent.{ListenableFuture, MoreExecutors}
import deaktator.advtut.futures.cats._
import deaktator.advtut.futures.instances.ListenableFutureInstances.Implicits.listenableFutureInstances
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.BlockJUnit4ClassRunner

import scala.language.higherKinds

/**
  * Created by ryan on 9/9/16.
  */
@RunWith(classOf[BlockJUnit4ClassRunner])
class FuturesTest {
  import FuturesTest._

  @Test def test(): Unit = {

    // An implicit ListeningExecutorService.
    implicit val exSvc =
      MoreExecutors.listeningDecorator(
        Executors.newFixedThreadPool(
          Runtime.getRuntime.availableProcessors()))

    // Get data inside a ListenableFuture.
    // Possible b/c the `Implicits._` import and implicit ListeningExecutorService.
    //
    // Same as:
    // val r = summingData(implicitly[Monad[ListenableFuture]])    // AND
    // val r = summingData(Monad[ListenableFuture])
    //
    // vvvvv   From Here on, we never mention ListenableFutures again.  Just some monad.  vvvvv
    val r = summingData[ListenableFuture]

    // vvvvv   These are the only places where blocking occurs.   vvvvv
    val res = r.results.get(5, SECONDS)  // BLOCK HERE!!!
    r.out.get(10, MILLISECONDS)          // BLOCK HERE (but don't really need to).
    // ^^^^^   These are the only places where blocking occurs.   ^^^^^

    val t2 = System.nanoTime
    testCorrectness(res)
    printTiming(r.t0, r.t1, t2)
  }

  /**
    * Get summing data not based on any particular monad instance.
    * @param anyMonad
    * @tparam M
    * @return
    */
  def summingData[M[_]](implicit anyMonad: Monad[M]) = {

    // Member functions can be imported.  Not necessary but useful.
    import anyMonad.{map2, point}

    val t0 = System.nanoTime

    val start = anyMonad.point(System.nanoTime)

    // Using lfm.point, asynchronously calculate the sum of 0 ... [Size - 1].
    val xSumAndTime: M[(BigInt, Long)] = point(andTime(sum(0, Size)))

    // Asynchronously calculate the sum of [Size] ... [1.5 * Size - 1] and add timing
    // information. This is half the size as xSumAndTime so it should be approximately
    // half the runtime too.
    val ySumAndTime = anyMonad.point(andTime(sum(Size, (Interval2Factor * Size).toInt)))

    // Asynchronously add x and y and timing information.
    val zSumAndTime = map2(xSumAndTime, ySumAndTime){case ((x, _), (y, _)) => andTime(x + y)}

    // Asynchronously construct the results.  `lfm` is passed implicitly.
    val results = combineResults(start, xSumAndTime, ySumAndTime, zSumAndTime)

    // Use `MonadOps` for the extension method `map` directly on the `results`.
    val out = results.map { r => println(r.mkString("\n", "\n", "\n")) }
    val t1 = System.nanoTime

    Results(results, out, t0, t1)
  }

  /**
    * Calculate the sum of `b` ... (`eExclusive` - 1) the dumb way.
    * @param b first element in the sum
    * @param eExclusive one after the last element in the sum.
    * @return
    */
  private[this] def sum(b: Int, eExclusive: Int): BigInt =
    (b until eExclusive).foldLeft(BigInt(0))((s, x) => s + x)

  /**
    * Combine the results and return inside a sequence wrapper in a monad.
    * @param start a start time.
    * @param xAndTime x and the time it was determined.
    * @param yAndTime y and the time it was determined.
    * @param zAndTime z and the time it was determined.
    * @tparam M the type of monad in which the values are embedded
    * @return A monad container with a sequence of string ID, computation time, results tuples.
    */
  private[this] def combineResults[M[_]: Monad](
      start: M[Long],
      xAndTime: M[(BigInt, Long)],
      yAndTime: M[(BigInt, Long)],
      zAndTime: M[(BigInt, Long)]): M[Seq[(String, Float, BigInt)]] = {

    for {
      st <- start
      xs <- xAndTime
      ys <- yAndTime
      zs <- zAndTime
    } yield {
      val (x, xt) = xs
      val (y, yt) = ys
      val (z, zt) = zs

      Seq(
        ("x", (1e-9*(xt - st)).toFloat, x),
        ("y", (1e-9*(yt - st)).toFloat, y),
        ("z", (1e-9*(zt - math.max(xt, yt))).toFloat, z)
      )
    }
  }

  private[this] def testCorrectness(actual: Seq[(String, Float, BigInt)]): Unit = {
    val m = BigInt(Size - 1)
    val n = BigInt((Interval2Factor * Size - 1).toLong)
    val expX = m * (m + 1) / 2
    val expZ = n * (n + 1) / 2
    val expY = expZ - expX

    actual match {
      case Seq(("x", xt, x), ("y", yt, y), ("z", zt, z)) =>
        assertEquals("for x: ", expX, x)
        assertEquals("for y: ", expY, y)
        assertEquals("for z: ", expZ, z)
      case _ => fail()
    }
  }

  private[this] def andTime[A](a: A): (A, Long) = (a, System.nanoTime())

  private[this] def printTiming(t0: Long, tReturn: Long, tFinish: Long): Unit = {
    val tr = (1e-9*(tReturn - t0)).toFloat
    val tf = (1e-9*(tFinish - t0)).toFloat
    println(s"time to get futures:      $tr seconds")
    println(s"time to complete futures: $tf seconds")
  }
}

object FuturesTest {
  val Size = 3.0e7.toInt
  val Interval2Factor = 1.5

  case class Results[M[_]](results: M[Seq[(String, Float, BigInt)]], out: M[Unit], t0: Long, t1: Long)
}
