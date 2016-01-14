package inverse_macros.concurrent

import debug._
import org.junit.Assert._
import org.scalatest.FunSuite

import scala.concurrent.Future
import scala.language.implicitConversions

class ConcurrentTest extends FunSuite {

  def sfib(n: Int): Int = if (n > 1) sfib(n - 1) + sfib(n - 2) else 1

  def strib(n: Int): Int = if (n > 2) strib(n - 1) + strib(n - 2) + strib(n - 3) else if (n == 2) 2 else 1

  def fib(n: Int, cutoff: Int): Int@monad[Future[Int]] =
    if (n < cutoff)
      sfib(n)
    else {
      val y = fork(fib(n - 2, cutoff))
      fib(n - 1, cutoff) + y
    }

  def trib(n: Int, cutoff: Int): Int@monad[Future[Int]] =
    if (n < cutoff)
      strib(n)
    else {
      val y = fork(trib(n - 2, cutoff))
      val z = fork(trib(n - 3, cutoff))
      trib(n - 1, cutoff) + y + z
    }

  def fibi(n: Int, cutoff: Int): Int@monad[Future[Int]] =
    if (n < cutoff)
      sfib(n)
    else
      fibi(n - 1, cutoff) + fork(fibi(n - 2, cutoff))

  def tribi(n: Int, cutoff: Int): Int@monad[Future[Int]] =
    if (n < cutoff)
      strib(n)
    else
      fork(tribi(n - 1, cutoff) + tribi(n - 2, cutoff)) + fork(tribi(n - 3, cutoff))

  test("fib") {
    for (n <- 1 to 30)
      inverse_macros.transform {
        assert(sfib(n) == sync(fib(n, 2)))
      }
    for (n <- 1 to 30)
      inverse_macros.transform {
        assert(sfib(n) == sync(fibi(n, 2)))
      }
  }

  test("trib") {
    for (n <- 1 to 20)
      inverse_macros.transform {
        assert(strib(n) == sync(trib(n, 3)))
      }
    for (n <- 1 to 20)
      inverse_macros.transform {
        assert(strib(n) == sync(tribi(n, 3)))
      }
  }
}
