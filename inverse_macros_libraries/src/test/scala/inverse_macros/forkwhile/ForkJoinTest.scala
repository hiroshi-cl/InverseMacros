package inverse_macros.forkwhile

import org.scalatest.FunSuite

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.language.implicitConversions

class ForkJoinTest extends FunSuite {

  val sb = new StringBuilder

  implicit class Sync(f: Future[Unit]) {
    def sync = {
      sb.append("join;")
      Await.result(f, Duration.Inf)
    }
  }

  def hoge: Unit = {
    var i = 0
    while (i < 10) {
      fork(sb.append(i).append(";"))
      scala.concurrent.blocking {
        Thread.sleep(1)
      }
      i += 1
    }
    sb.append("finish;")
  }

  test("fork/join") {
    inverse_macros.transform {
      hoge
      assert(sb.toString == "0;1;2;3;4;5;6;7;8;9;join;join;join;join;join;join;join;join;join;join;finish;")
    }
  }
}
