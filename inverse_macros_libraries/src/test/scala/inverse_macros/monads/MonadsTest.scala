package inverse_macros.monads

import debug._
import org.junit.Assert._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class MonadsTest extends FunSuite {
  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  import Monads._

  test("option") {
    import OptionMonad._
    assert((try {None.reflect; ???} catch {
      case e: MonadContext[_] => e.m.asInstanceOf[Option[Int]]
    }).isEmpty)
  }

  test("reader") {
    import StateMonad._
    assert((try {
      ((x: Int) => (x, x + 1)).reflect
    } catch {
      case e: MonadContext[_] => e.m.asInstanceOf[Int =>(Int, Int)](10)._2
    }) == 11)
  }

  test("writer") {
    import StateMonad._
    assert(implicitly[MUnit[Int, Int => (Int, Int)]].unit(10)
      .flatMap(x => _ => (x, x))
      .flatMap(x => _ => (20, x))
      .apply(0)._1 == 20)
  }


}
