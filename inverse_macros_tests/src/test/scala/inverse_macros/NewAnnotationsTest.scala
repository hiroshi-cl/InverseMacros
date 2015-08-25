package inverse_macros

import debug._
import org.scalatest.FunSuite

class NewAnnotationsTest extends FunSuite {
  def intAbort: Int@abort = ???

  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  test("abort -") {
    assert(expectException(parse("transform(println(intPrintln))")))
  }

  test("abort +") {
    //    compare(show(transform(intAnnotFunc(intAnnot))))("ByNamesTest.this.intAnnotFunc(ByNamesTest.this.intAnnot)")
    //    compare(show(transform(intAnnotFunc(int))))("ByNamesTest.this.intAnnotFunc(ByNamesTest.this.int)")
  }

}
