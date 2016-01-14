package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class Suspendable extends JUnitSuite  {
  def shifted: Unit @suspendable = shift { (k: Unit => Unit) => () }
  def test1(b: => Boolean) = {
    reset[Unit,Unit] {
      if (b) shifted
    }
  }
  @Test def t1820 = test1(true)

  def suspended[A](x: A): A @suspendable = x
  def test1[A](x: A): A @suspendable = suspended(x) match { case x => x }
  def test2[A](x: List[A]): A @suspendable = suspended(x) match { case List(x) => x }

  def test3[A](x: A): A @suspendable = x match { case x => x }
  def test4[A](x: List[A]): A @suspendable = x match { case List(x) => x }

  @Test def t1821 {
    assertEquals((), reset[Unit,Unit](test1(())))
    assertEquals((), reset[Unit,Unit](test2(List(()))))
    assertEquals((), reset[Unit,Unit](test3(())))
    assertEquals((), reset[Unit,Unit](test4(List(()))))
  }
}