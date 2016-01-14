package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class PatternMatching extends JUnitSuite  {

  def test(x: Int) = x match {
    case 7 => shift { k: (Int => Int) => k(k(k(x))) }
    case 8 => shift { k: (Int => Int) => k(x) }
  }

  @Test def match0 = {
    assertEquals(10, reset[Int,Int](1 + test(7)))
    assertEquals(9, reset[Int,Int](1 + test(8)))
  }

  def test1(x: Int) = x match {
    case 7 => shift { k: (Int => Int) => k(k(k(x))) }
    case _ => x
  }

  @Test def match1 = {
    assertEquals(10, reset[Int,Int](1 + test(7)))
    assertEquals(9, reset[Int,Int](1 + test(8)))
  }

  def test2() = {
    val (a, b) = shift { k: (((String, String)) => String) => k("A", "B") }
    b
  }

  case class Elem[T, U](a: T, b: U)

  def test3() = {
    val Elem(a, b) = shift { k: (Elem[String, String] => String) => k(Elem("A", "B")) }
    b
  }

  @Test def match2 = {
    assertEquals("B", reset[String,String](test2()))
    assertEquals("B", reset[String,String](test3()))
  }

  def sh(x1: Int) = shift((k: Int => Int) => k(k(k(x1))))

  def testv(x1: Int) = {
    val o7 = {
      val o6 = {
        val o3 =
          if (7 == x1) Some(x1)
          else None

        if (o3.isEmpty) None
        else Some(sh(x1))
      }
      if (o6.isEmpty) {
        val o5 =
          if (8 == x1) Some(x1)
          else None

        if (o5.isEmpty) None
        else Some(sh(x1))
      } else o6
    }
    o7.get
  }

  @Test def patvirt = {
    assertEquals(10, reset[Int,Int](1 + testv(7)))
    assertEquals(11, reset[Int,Int](1 + testv(8)))
  }

  class MatchRepro {
    def s: String @cps[Any] = shift[String, Any, Any] { k => k("foo") }

    def p = {
      val k = s
      s match { case lit0 => }
    }

    def q = {
      val k = s
      k match { case lit1 => }
    }

    def r = {
      s match { case "FOO" => }
    }

    def t = {
      val k = s
      k match { case "FOO" => }
    }
  }

  @Test def z1673 = {
    val m = new MatchRepro
    ()
  }
}
