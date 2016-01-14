package inverse_macros.concurrent

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object FibSync {

  def sfib(n: Int): Int = if (n > 1) sfib(n - 1) + sfib(n - 2) else 1

  def strib(n: Int): Int = if (n > 2) strib(n - 1) + strib(n - 2) + strib(n - 3) else if (n == 2) 2 else 1

  def fib(n: Int, cutoff: Int): Int = {
    if (n < cutoff)
      sfib(n)
    else {
      val xf = Future(fib(n - 1, cutoff))
      val yf = Future(fib(n - 2, cutoff))
      Await.result(xf, Duration.Inf) + Await.result(yf, Duration.Inf)
    }
  }

  def trib(n: Int, cutoff: Int): Int = {
    if (n < cutoff)
      strib(n)
    else {
      val xf = Future(trib(n - 1, cutoff))
      val yf = Future(trib(n - 2, cutoff))
      val zf = Future(trib(n - 3, cutoff))
      Await.result(xf, Duration.Inf) + Await.result(yf, Duration.Inf)+ Await.result(zf, Duration.Inf)
    }
  }

  @inline def runFib(n: Int, cutoff: Int): Int = fib(n, cutoff)

  @inline def runTrib(n: Int, cutoff: Int): Int = trib(n, cutoff)
}
