package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class Functions extends JUnitSuite {

  def m0() = {
    shift((k: Int => Int) => k(k(7))) * 2
  }

  def m1() = {
    2 * shift((k: Int => Int) => k(k(7)))
  }

  @Test def basics = {

    assertEquals(28, reset[Int,Int](m0()))
    assertEquals(28, reset[Int,Int](m1()))
  }

  @Test def function1 = {

    val f = () => shift { k: (Int => Int) => k(7) }
    val g: () => Int @cps[Int] = f

    assertEquals(7, reset[Int,Int](g()))
  }

  @Test def function4 = {

    val g: () => Int @cps[Int] = () => shift { k: (Int => Int) => k(7) }

    assertEquals(7, reset[Int,Int](g()))
  }

  @Test def function5 = {

    val g: () => Int @cps[Int] = () => 7

    assertEquals(7, reset[Int,Int](g()))
  }

  @Test def function6 = {

    val g: PartialFunction[Int, Int @cps[Int]] = { case x => 7 }

    assertEquals(7, reset[Int,Int](g(2)))

  }

}
