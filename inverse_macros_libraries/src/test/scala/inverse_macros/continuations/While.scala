package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

 // while1, while2 のために type ascription を加えた

class While extends JUnitSuite  {
  val out = new StringBuilder; def printOut(x: Any): Unit = out ++= x.toString

  def foo0(): Int @cps[Unit] = 2

  def test0(): Unit @cps[Unit] = {
    var x = 0
    while (x < 9000) { // pick number large enough to require tail-call opt
      x += foo0()
    }
    assertEquals(9000, x)
  }

  @Test def while0 = {
    reset[Unit,Unit](test0())
  }

  // addded type ascription
  def foo3(): Int @cps[Unit] = shift { (k: Int => Unit) => printOut("up"); k(2); printOut("down") }

  def test3(): Unit @cps[Unit] = {
    var x = 0
    while (x < 9) {
      x += foo3()
    }
    printOut(x)
  }

  @Test def while1 = {
    out.clear
    reset[Unit,Unit](test3())
    assertEquals("upupupupup10downdowndowndowndown", out.toString)
  }

  def foo1(): Int @cps[Unit] = 2
  // addded type ascription
  def foo2(): Int @cps[Unit] = shift { (k: Int => Unit) => printOut("up"); k(2); printOut("down") }

  def test2(): Unit @cps[Unit] = {
    var x = 0
    while (x < 9000) { // pick number large enough to require tail-call opt
      x += (if (x % 1000 != 0) foo1() else foo2())
    }
    printOut(x)
  }

  @Test def while2 = {
    out.clear
    reset[Unit,Unit](test2())
    assertEquals("upupupupupupupupup9000downdowndowndowndowndowndowndowndown", out.toString)
  }
}

