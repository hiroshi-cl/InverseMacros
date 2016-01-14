package inverse_macros

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

package object forkwhile {

  implicit class Sync(f: Future[Unit]) {
    def sync = Await.result(f, Duration.Inf)
  }

}
