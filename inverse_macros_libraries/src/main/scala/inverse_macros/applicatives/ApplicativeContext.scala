package inverse_macros.applicatives

import scala.util.control.ControlThrowable

final case class ApplicativeContext[MA](m: MA) extends ControlThrowable
