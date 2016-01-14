package inverse_macros.concurrent

import scala.util.control.ControlThrowable

final case class ConcurrentContext[MA](m: MA) extends ControlThrowable
