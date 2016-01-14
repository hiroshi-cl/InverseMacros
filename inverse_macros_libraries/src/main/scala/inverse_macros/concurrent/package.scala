package inverse_macros

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise, Future}

package object concurrent {

  import scala.concurrent.ExecutionContext.Implicits.global

  def unit[X](x: X) = Future.successful(x)

  def fork[X](body: => X@monad[Future[X]]): X@lzy[Future[X]] = {
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
    throw ConcurrentContext(p.future)
  }

  def sync[X](body: => X@monad[Future[X]]): X = try {
    @IMSynth val b: X = body
    b
  } catch {
    case ex: ConcurrentContext[_] => Await.result(ex.m.asInstanceOf[Future[X]], Duration.Inf)
  }

  def syncLzy[X](body: => X@lzy[Future[X]]): X = try {
    @IMSynth val b: X = body
    b
  } catch {
    case ex: ConcurrentContext[_] => Await.result(ex.m.asInstanceOf[Future[X]], Duration.Inf)
  }

  implicit class FutureOps[X](val cx: Future[X]) extends AnyVal {
    def fmap[R](f: X => R): R@monad[Future[R]] = throw ConcurrentContext(cx.map(f))

    def bind[R](f: X => R@monad[Future[R]]): R@monad[Future[R]] = {
      val p = Promise[R]
      cx.onSuccess {
        case x =>
          try p.success {
            @IMSynth val v: R = f(x)
            v
          } catch {
            case ex: ConcurrentContext[_] => ex.m.asInstanceOf[Future[R]].onSuccess {
              case y: R => p.success(y)
            }
          }
      }
      throw ConcurrentContext(p.future)
    }

    def jget(): X@monad[Future[X]] = throw ConcurrentContext(cx)
  }

}
