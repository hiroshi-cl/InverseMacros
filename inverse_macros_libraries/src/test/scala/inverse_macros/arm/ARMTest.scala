package inverse_macros.arm

import org.scalatest.FunSuite

import scala.language.implicitConversions

class ARMTest extends FunSuite {

  test("arm") {
    val sb = new StringBuilder

    object resource {
      def close = sb.append("closed;")
    }

    def hoge: Unit = {
      val r = arm(resource)
      sb.append("using r;")
      println(r)
    }

    inverse_macros.transform {
      hoge
      assert(sb.toString == "using r;closed;")
    }
  }
}
