package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class IfReturn extends JUnitSuite {
  // d = 1, d2 = 1.0, pct = 1.000
  // d = 2, d2 = 4.0, pct = 0.500
  // d = 3, d2 = 9.0, pct = 0.333
  // d = 4, d2 = 16.0, pct = 0.250
  // d = 5, d2 = 25.0, pct = 0.200
  // d = 6, d2 = 36.0, pct = 0.167
  // d = 7, d2 = 49.0, pct = 0.143
  // d = 8, d2 = 64.0, pct = 0.125
  // d = 9, d2 = 81.0, pct = 0.111
  // d = 10, d2 = 100.0, pct = 0.100
  // d = 11, d2 = 121.0, pct = 0.091
  // d = 12, d2 = 144.0, pct = 0.083
  // d = 13, d2 = 169.0, pct = 0.077
  // d = 14, d2 = 196.0, pct = 0.071
  // d = 15, d2 = 225.0, pct = 0.067
  // d = 16, d2 = 256.0, pct = 0.063
  // d = 17, d2 = 289.0, pct = 0.059
  // d = 18, d2 = 324.0, pct = 0.056
  // d = 19, d2 = 361.0, pct = 0.053
  // d = 20, d2 = 400.0, pct = 0.050
  // d = 21, d2 = 441.0, pct = 0.048
  // d = 22, d2 = 484.0, pct = 0.045
  // d = 23, d2 = 529.0, pct = 0.043
  // d = 24, d2 = 576.0, pct = 0.042
  // d = 25, d2 = 625.0, pct = 0.040

  abstract class IfReturnRepro {
    def s1: Double @cpsParam[Any, Unit]
    def s2: Double @cpsParam[Any, Unit]

    def p(i: Int): Double @cpsParam[Unit, Any] = {
      val px = s1
      val pct = if (px > 100) px else px / s2
      //printOut("pct = %.3f".format(pct))
      assertEquals(s1 / s2, pct, 0.0001)
      pct
    }
  }

  // TODO: failed by type error
  @Test def shift_pct = {
    var d: Double = 0d
    def d2 = d * d

    val irr = new IfReturnRepro {
      def s1 = shift[Double, Any, Unit](f => f(d))
      def s2 = shift[Double, Any, Unit](f => f(d2))
    }
    1 to 25 foreach { i =>
      d = i
      // print("d = " + i + ", d2 = " + d2 + ", ")
      assertEquals(i.toDouble * i, d2, 0.1)
//      inverse_macros.continuations.run[Double](irr p i) // expected to be failed by type error (2.12) but not failed (2.11)
      inverse_macros.continuations.run[Unit](irr p i)
    }
  }

  @Test def t1807 = {
    val z = reset[Int,Int] {
      val f: (() => Int @cps[Int]) = () => 1
      f()
    }
    assertEquals(1, z)
  }

  @Test def t1808: Unit = {
    reset0 { 0 }
  }
}

