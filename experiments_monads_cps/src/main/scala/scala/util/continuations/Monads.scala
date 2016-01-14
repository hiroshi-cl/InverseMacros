package scala.util.continuations

trait MUnit[-X, +CX] {
  @inline def unit(x: X): CX
}

object OptionMonad {
  implicit def optionMUnit[X] = new MUnit[X, Option[X]] {
    @inline def unit(x: X): Option[X] = Some(x)
  }

  implicit class OptionMReflect[X](val cx: Option[X]) extends AnyVal {
    @inline def reflect: X@cps[Option[Any]] = if(cx.isDefined) cx.get else shift(cx.flatMap(_))
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

  @inline def get[S]: S@cps[S => (S, Any)] = shift(((s: S) => (s, s)).flatMap(_))

  @inline def put[S](x: S): Unit@cps[S => (S, Any)] = shift((k: Unit => S => (S, Any)) => ((_: S) => (x, ())).flatMap(k))

  implicit def stateMUnit[S, X] = new MUnit[X, S => (S, X)] {
    def unit(x: X): S => (S, X) = (s: S) => (s, x)
  }

  implicit class StateMReflect[S, X](val cx: S => (S, X)) extends AnyVal {
    @inline def reflect: X@cps[S => (S, Any)] = shift(cx.flatMap(_))
  }

}


object Monads {
  @inline def reify[X, CX](body: => X@cpsParam[CX, Any])(implicit munit: MUnit[X, CX]): CX = reset {
    munit.unit(body)
  }.asInstanceOf[CX]
}