package inverse_macros.continuations

import scala.util.control.ControlThrowable

final case class ControlContext[+A, -B, +C](ctl: (A => B) => C) extends ControlThrowable {
  @inline def map[A1](ctn1: A => A1): ControlContext[A1, B, C] =
    ControlContext((ctn2: A1 => B) => ctl((x: A) => ctn2(ctn1(x))))

  @inline def flatMap[A1, B1, C1 <: B](ctn1: A => A1@cpsParam[B1, C1]): ControlContext[A1, B1, C] = {
    @inverse_macros.IMSynth val c = ControlContext((ctn2: A1 => B1) => ctl((x: A) => try {
      ctn2(ctn1(x)).asInstanceOf[B]
    } catch {
      case ctx: ControlContext[_, _, _] =>
        ctx.asInstanceOf[ControlContext[A1, B1, C1]].ctl(ctn2)
    }))
    c
  }
}
