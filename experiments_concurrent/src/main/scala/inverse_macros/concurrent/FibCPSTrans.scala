package inverse_macros.concurrent

import inverse_macros.IMSynth

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object FibCPSTrans {

  def sfib(n: Int): Int = if (n > 1) sfib(n - 1) + sfib(n - 2) else 1

  def strib(n: Int): Int = if (n > 2) strib(n - 1) + strib(n - 2) + strib(n - 3) else if (n == 2) 2 else 1

  def async[X](body: => X@monad[Future[X]]): Future[X] = {
    val p = Promise[X]
    Future {
      try p.success {
        @IMSynth val b: X = body
        b
      } catch {
        case ex: ConcurrentContext[_] => ex.m.asInstanceOf[Future[X]].onSuccess {
          case x: X => p.success(x)
        }
      }
    }
    p.future
  }

  def await[X](f: Future[X]): X@monad[Future[X]] = throw ConcurrentContext(f)

  def fib(n: Int, cutoff: Int): Future[Int] = async {
    if (n < cutoff)
      sfib(n)
    else{
      val xf = fib(n - 1, cutoff)
      val yf = fib(n - 2, cutoff)
      await(xf) + await(yf)
    }

  }

  def trib(n: Int, cutoff: Int): Future[Int] = async {
    if (n < cutoff)
      strib(n)
    else{
      val xf = trib(n - 1, cutoff)
      val yf = trib(n - 2, cutoff)
      val zf = trib(n - 3, cutoff)
      await(xf) + await(yf) + await(zf)
    }
  }

  @inline def runFib(n: Int, cutoff: Int): Int = Await.result(fib(n, cutoff), Duration.Inf)

  @inline def runTrib(n: Int, cutoff: Int): Int = Await.result(trib(n, cutoff), Duration.Inf)
}
