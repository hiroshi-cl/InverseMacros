package inverse_macros.monads

trait MUnit[-X, +CX] {
  @inline def unit(x: X): CX
}

object OptionMonad {
  implicit def optionMUnit[X] = new MUnit[X, Option[X]] {
    @inline def unit(x: X): Option[X] = Some(x)
  }

  implicit class OptionMReflect[X](val cx: Option[X]) extends AnyVal {
    @inline def reflect: X@monad[Option[X]] = if (cx.isDefined) cx.get else throw MonadContext(cx)
  }

}

object StateMonad {

  implicit class State[S, +X](val st: S => (S, X)) extends AnyVal {
    def map[R](f: X => R): (S => (S, R)) = (s: S) => {
      val t = st(s)
      (t._1, f(t._2))
    }

    def flatMap[R](f: X => (S => (S, R))): (S => (S, R)) = (s: S) => {
      val t = st(s)
      f(t._2)(t._1)
    }
  }

  @inline def get[S]: S@monad[S => (S, S)] = throw MonadContext((s: S) => (s, s))

  @inline def put[S](x: S): Unit@monad[S => (S, Unit)] = throw MonadContext((s: S) => (x, ()))

  implicit def stateMUnit[S, X] = new MUnit[X, S => (S, X)] {
    def unit(x: X): S => (S, X) = (s: S) => (s, x)
  }

  implicit class StateMReflect[S, X](val cx: S => (S, X)) extends AnyVal {
    @inline def reflect: X@monad[S => (S, X)] = throw MonadContext(cx)
  }

}

object Monads {

  @inline def reify[X, CX](body: => X@monad[CX])(implicit munit: MUnit[X, CX]): CX = {
    @inverse_macros.IMSynth val c = try {
      val v = body; munit.unit(v)
    } catch {
      case e: MonadContext[_] => e.m.asInstanceOf[CX]
    }
    c
  }
}