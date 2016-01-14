package inverse_macros.forkwhile

import scala.concurrent.Future
import scala.util.control.ControlThrowable

final case class ForkContext(list: List[Future[Unit]]) extends ControlThrowable
