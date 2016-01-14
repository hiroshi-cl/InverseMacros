package inverse_macros.applicatives

import scala.concurrent.Future

object Fib {

  import FutureApplicative._
  import scala.concurrent.ExecutionContext.Implicits.global

  def sfib(n: Int): Int = if (n > 1) sfib(n - 1) + sfib(n - 2) else 1

  def strib(n: Int): Int = if (n > 2) strib(n - 1) + strib(n - 2) + strib(n - 3) else if (n == 2) 2 else 1

  def fib(n: Int, cutoff: Int): Int@applicative[Future[Int]] = fork {
    if (n < cutoff)
      sfib(n)
    else
      fib(n - 1, cutoff) + fib(n - 2, cutoff)
  }

  def trib(n: Int, cutoff: Int): Int@applicative[Future[Int]] = fork {
    if (n < cutoff)
      strib(n)
    else
      trib(n - 1, cutoff) + trib(n - 2, cutoff) + trib(n - 3, cutoff)
  }

  @inline def runFib(n: Int, cutoff: Int): Int = sync(fib(n, cutoff))

  @inline def runTrib(n: Int, cutoff: Int): Int = sync(trib(n, cutoff))
}
