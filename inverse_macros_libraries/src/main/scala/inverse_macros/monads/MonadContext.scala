package inverse_macros.monads

import scala.util.control.ControlThrowable

final case class MonadContext[MA](m: MA) extends ControlThrowable
