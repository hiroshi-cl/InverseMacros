package inverse_macros.concurrent

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object FibCPS {

  def sfib(n: Int): Int = if (n > 1) sfib(n - 1) + sfib(n - 2) else 1

  def strib(n: Int): Int = if (n > 2) strib(n - 1) + strib(n - 2) + strib(n - 3) else if (n == 2) 2 else 1

  def fib(n: Int, cutoff: Int): Future[Int] = {
    if (n < cutoff)
      Future(sfib(n))
    else {
      val xf = fib(n - 1, cutoff)
      val yf = fib(n - 2, cutoff)
      xf.flatMap(x => yf.map(y => x + y))
    }
  }

  def trib(n: Int, cutoff: Int): Future[Int] = {
    if (n < cutoff)
      Future(strib(n))
    else {
      val xf = trib(n - 1, cutoff)
      val yf = trib(n - 2, cutoff)
      val zf = trib(n - 3, cutoff)
      xf.flatMap(x => yf.flatMap(y => zf.map(z => x + y + z)))
    }
  }

  @inline def runFib(n: Int, cutoff: Int): Int = Await.result(fib(n, cutoff), Duration.Inf)

  @inline def runTrib(n: Int, cutoff: Int): Int = Await.result(trib(n, cutoff), Duration.Inf)
}
