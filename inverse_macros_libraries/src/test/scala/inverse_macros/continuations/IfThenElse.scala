package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class IfThenElse extends JUnitSuite  {
  val out = new StringBuilder; def printOut(x: Any): Unit = out ++= x.toString

  def test(x: Int) = if (x <= 7)
    shift { k: (Int => Int) => k(k(k(x))) }
  else
    shift { k: (Int => Int) => k(x) }

  @Test def ifelse0 = {
    assertEquals(10, reset[Int,Int](1 + test(7)))
    assertEquals(9, reset[Int,Int](1 + test(8)))
  }

  def test1(x: Int) = if (x <= 7)
    shift { k: (Int => Int) => k(k(k(x))) }
  else
    x

  def test2(x: Int) = if (x <= 7)
    x
  else
    shift { k: (Int => Int) => k(k(k(x))) }

  @Test def ifelse1 = {
    assertEquals(10, reset[Int,Int](1 + test1(7)))
    assertEquals(9, reset[Int,Int](1 + test1(8)))
    assertEquals(8, reset[Int,Int](1 + test2(7)))
    assertEquals(11, reset[Int,Int](1 + test2(8)))
  }

  def test3(x: Int) = if (x <= 7)
    shift { k: (Unit => Unit) => printOut("abort") }

  @Test def ifelse2 = {
    out.clear()
    printOut(reset[Unit,Unit] { test3(7); printOut("alive") })
    printOut(reset[Unit,Unit] { test3(8); printOut("alive") })
    assertEquals("abort()alive()", out.toString)
  }

  def util(x: Boolean) = shift { k: (Boolean => Int) => k(x) }

  def test4(x: Int) = if (util(x <= 7))
    x - 1
  else
    x + 1

  @Test def ifelse3 = {
    assertEquals(6, reset[Int,Int](test4(7)))
    assertEquals(9, reset[Int,Int](test4(8)))
  }

  def sh(x1: Int) = shift((k: Int => Int) => k(k(k(x1))))

  def testA(x1: Int): Int @cps[Int] = {
    sh(x1)
    if (x1 == 42) x1 else sh(x1)
  }

  def testB(x1: Int): Int @cps[Int] = {
    if (sh(x1) == 43) x1 else x1
  }

  def testC(x1: Int): Int @cps[Int] = {
    sh(x1)
    if (sh(x1) == 44) x1 else x1
  }

  def testD(x1: Int): Int @cps[Int] = {
    sh(x1)
    if (sh(x1) == 45) x1 else sh(x1)
  }

  @Test def ifelse4 = {
    assertEquals(10, reset[Int,Int](1 + testA(7)))
    assertEquals(10, reset[Int,Int](1 + testB(9)))
    assertEquals(10, reset[Int,Int](1 + testC(9)))
    assertEquals(10, reset[Int,Int](1 + testD(7)))
  }
}
