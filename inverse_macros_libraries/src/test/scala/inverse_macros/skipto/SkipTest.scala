package inverse_macros.skipto

import org.scalatest.FunSuite

import scala.language.implicitConversions

class SkipTest extends FunSuite {

  val sb = new StringBuilder

  test("skipto") {
    inverse_macros.transform {
      sb.append("start;")
      skipto("hoge")
      sb.append("skipped 1")
      label("piyo")
      sb.append("skipped 2")
      label("hoge")
      sb.append("finished;")
    }
    assert(sb.toString() == "start;finished;")
  }
}
