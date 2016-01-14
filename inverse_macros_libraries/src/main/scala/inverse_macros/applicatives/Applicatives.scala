package inverse_macros.applicatives

trait AUnit[-X, +CX] {
  @inline def unit(x: X): CX
}

object StateApplicative {

  import StateApplicative._

  implicit class State[S, +X](val st: S => (S, X)) extends AnyVal {
    def map[R](f: X => R): (S => (S, R)) = (s: S) => {
      val t = st(s)
      (t._1, f(t._2))
    }

    //    def flatMap[R](f: X => (S => (S, R))): (S => (S, R)) = (s: S) => {
    //      val t = st(s)
    //      f(t._2)(t._1)
    //    }

    def ap[R](f: S => (S, X => R)) = (s: S) => {
      val fs = f(s)
      val t = st(fs._1)
      (t._1, fs._2(t._2))
    }
  }

  @inline def get[S]: S@applicative[S => (S, S)] = throw ApplicativeContext((s: S) => (s, s))

  @inline def put[S](x: S): Unit@applicative[S => (S, Unit)] = throw ApplicativeContext((s: S) => (x, ()))

  implicit def stateAUnit[S, X] = new AUnit[X, S => (S, X)] {
    def unit(x: X): S => (S, X) = (s: S) => (s, x)
  }

  implicit class StateAReflect[S, X](val cx: S => (S, X)) extends AnyVal {
    @inline def reflect: X@applicative[S => (S, X)] = throw ApplicativeContext(cx)
  }

}


object FutureApplicative {

  import inverse_macros.IMSynth

  import scala.concurrent.Await
  import scala.concurrent.duration.Duration
  import scala.concurrent.{Promise, Future}
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit class FutureOps[X](val cx: Future[X]) extends AnyVal {

    def ap[R](cf: Future[X => R]): Future[R] = {
      val p = Promise[R]
      cx.onSuccess {
        case x =>
          cf.onSuccess {
            case f =>
              p.success(f(x))
          }
      }
      p.future
    }
  }

  implicit def futureAUnit[S, X] = new AUnit[X, Future[X]] {
    def unit(x: X): Future[X] = Future.successful(x)
  }

  def fork[X](body: => X@applicative[Future[X]]): X@applicative[Future[X]] = {
    val p = Promise[X]
    Future {
      try p.success {
        @IMSynth val b: X = body
        b
      } catch {
        case ex: ApplicativeContext[_] => ex.m.asInstanceOf[Future[X]].onSuccess {
          case x: X => p.success(x)
        }
      }
    }
    throw ApplicativeContext(p.future)
  }

  def sync[X](body: => X@applicative[Future[X]]): X = try {
    @IMSynth val b: X = body
    b
  } catch {
    case ex: ApplicativeContext[_] => Await.result(ex.m.asInstanceOf[Future[X]], Duration.Inf)
  }
}


object Applicatives {

  @inline def reify[X, CX](body: => X@applicative[CX])(implicit aunit: AUnit[X, CX]): CX = {
    @inverse_macros.IMSynth val c = try {
      val v = body; aunit.unit(v)
    } catch {
      case e: ApplicativeContext[_] => e.m.asInstanceOf[CX]
    }
    c
  }
}
