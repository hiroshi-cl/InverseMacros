package inverse_macros.lazys

import scala.util.control.ControlThrowable

final case class LazyContext[+A](body: () => A) extends ControlThrowable