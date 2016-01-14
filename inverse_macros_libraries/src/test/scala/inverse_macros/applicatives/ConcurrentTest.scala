package inverse_macros.applicatives

import org.scalatest.FunSuite

import scala.concurrent.Future
import scala.language.implicitConversions

class ConcurrentTest extends FunSuite {

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

  test("fib") {
    for (n <- 1 to 30)
      inverse_macros.transform {
        assert(sfib(n) == sync(fib(n, 2)))
      }
  }

  test("trib") {
    for (n <- 1 to 20)
      inverse_macros.transform {
        assert(strib(n) == sync(trib(n, 3)))
      }
  }
}
