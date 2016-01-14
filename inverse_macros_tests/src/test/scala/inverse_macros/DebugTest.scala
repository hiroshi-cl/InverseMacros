package inverse_macros

import debug._
import org.scalatest.FunSuite

class DebugTest extends FunSuite {

//  test("abort") {
//    assert(expectException(parse("abort")))
//  }
  test("normal function") {
    assert(show(applied(1)("to")(10, 1)) == "(scala.Predef.intWrapper(1).to(10, 1): Any)")
  }
  test("symbolic name function") {
    assert(show(applied(1)("+")(1)) == "(2: Any)")
  }
  test("normal function (show)") {
    assert(show(applied(1)("to")(10, 1)) == show(1 to(10, 1) : Any))
  }
  test("symbolic name function (show)") {
    assert(show(applied(1)("+")(1)) == show(1 + 1 : Any))
  }
  test("parse") {
    assert(parse("10") == 10)
    assert(parse("10 + 29") == 39)
  }
}