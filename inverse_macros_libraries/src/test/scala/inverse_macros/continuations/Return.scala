package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class Return extends JUnitSuite  {
  val out = new StringBuilder; def printOut(x: Any): Unit = out ++= x.toString

  class ReturnRepro {
    def s1: Int @cps[Any] = shift[Int, Any, Any] { k => k(5) }
    def caller = reset[Any,Any] { printOut(p(3)) }
    def caller2 = reset[Any,Any] { printOut(p2(3)) }
    def caller3 = reset[Any,Any] { printOut(p3(3)) }

    def p(i: Int): Int @cps[Any] = {
      val v = s1 + 3
      return v
    }

    def p2(i: Int): Int @cps[Any] = {
      val v = s1 + 3
      if (v > 0) {
        printOut("hi")
        return v
      } else {
        printOut("hi")
        return 8
      }
    }

    def p3(i: Int): Int @cps[Any] = {
      val v = s1 + 3
      try {
        printOut("from try")
        return v
      } catch {
        case e: Exception =>
          printOut("from catch")
          return 7
      }
    }
  }

  @Test def t5314_2 = {
    out.clear()
    val repro = new ReturnRepro
    repro.caller
    repro.caller2
    repro.caller3
    assertEquals("8hi8from try8", out.toString)
  }

  class ReturnRepro2 {

    def s1: Int @cpsParam[Any, Unit] = shift[Int, Any, Unit] { k => k(5) }
    def caller = reset[Unit,Any] { printOut(p(3)) }
    def caller2 = reset[Unit,Any] { printOut(p2(3)) }

    def p(i: Int): Int @cpsParam[Unit, Any] = {
      val v = s1 + 3
      return { printOut("enter return expr"); v }
    }

    def p2(i: Int): Int @cpsParam[Unit, Any] = {
      val v = s1 + 3
      if (v > 0) {
        return { printOut("hi"); v }
      } else {
        return { printOut("hi"); 8 }
      }
    }
  }

  @Test def t5314_3 = {
    out.clear()
    val repro = new ReturnRepro2
    repro.caller
    repro.caller2
    assertEquals("enter return expr8hi8", out.toString)
  }

  def foo(x: Int): Int @cps[Int] = 7

  def bar(x: Int): Int @cps[Int] = {
    val v = foo(x)
    if (v > 0)
      return v
    else
      return 10
  }

  @Test def t5314_with_if =
    assertEquals(7, reset[Int,Int] { bar(10) })

}

class t5314 extends JUnitSuite  {
  val out = new StringBuilder; def printOut(x: Any): Unit = out ++= x.toString

  class ReturnRepro3 {
    def s1: Int @cpsParam[Any, Unit] = shift[Int, Any, Unit] { k => k(5) }
    def caller = reset[Unit,Any] { printOut(p(3)) }
    def caller2 = reset[Unit,Any] { printOut(p2(3)) }

    def p(i: Int): Int @cpsParam[Unit, Any] = {
      val v = s1 + 3
      return v
    }

    def p2(i: Int): Int @cpsParam[Unit, Any] = {
      val v = s1 + 3
      if (v > 0) {
        printOut("hi")
        return v
      } else {
        printOut("hi")
        return 8
      }
    }
  }

  def foo(x: Int): Int @cps[Int] = shift[Int, Int, Int] { k => k(x) }

  def bar(x: Int): Int @cps[Int] = return foo(x)

  def nocps(x: Int): Int = { return x; x }

  def foo2(x: Int): Int @cps[Int] = 7
  def bar2(x: Int): Int @cps[Int] = { foo2(x); return 7 }
  def bar3(x: Int): Int @cps[Int] = { foo2(x); if (x == 7) return 7 else return foo2(x) }
  def bar4(x: Int): Int @cps[Int] = { foo2(x); if (x == 7) return 7 else foo2(x) }
  def bar5(x: Int): Int @cps[Int] = { foo2(x); if (x == 7) return 7 else 8 }

  @Test def t5314 = {
    out.clear()

    printOut(reset[Int,Int] { bar2(10) })
    printOut(reset[Int,Int] { bar3(10) })
    printOut(reset[Int,Int] { bar4(10) })
    printOut(reset[Int,Int] { bar5(10) })

    /* original test case */
    val repro = new ReturnRepro3
    repro.caller
    repro.caller2

    reset[Int,Int] {
      val res = bar(8)
      printOut(res)
      res
    }

    assertEquals("77788hi88", out.toString)
  }

}