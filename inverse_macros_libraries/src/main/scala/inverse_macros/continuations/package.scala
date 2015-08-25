package inverse_macros

package object continuations {
  @inline final def shift[A, B, C](fun: (A => B) => C): A@cpsParam[B, C] = throw ControlContext(fun)

  final def reset[A, C](body: => A@cpsParam[A, C]): C = try {
    @IMSynth val b = body.asInstanceOf[C]
    b
  } catch {
    case ctx: ControlContext[_, _, _] => ctx.asInstanceOf[ControlContext[A, A, C]].ctl(identity)
  }

  type cps[T] = cpsParam[T, T]

  type suspendable = cpsParam[Unit, Unit]

  final def reset0[A](body: => A@cpsParam[A, A]): A = try {
    @IMSynth val b = body.asInstanceOf[A]
    b
  } catch {
    case ctx: ControlContext[_, _, _] => ctx.asInstanceOf[ControlContext[A, A, A]].ctl(identity)
  }

  final def run[A](body: => Any@cpsParam[Unit, A]): A = try {
    @IMSynth val b = body.asInstanceOf[A]
    b
  } catch {
    case ctx: ControlContext[_, _, _] => ctx.asInstanceOf[ControlContext[Any, Unit, A]].ctl(_ => ())
  }

  @inline final def shiftUnit0[A, B](a: A): A@cpsParam[B, B] = a
}
