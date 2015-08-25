package inverse_macros.lazys

import debug._
import org.junit.Assert._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class LazyTest extends FunSuite {
  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  test("simple @") {
    inverse_macros.transform {
      println("simple")
      10: @lzy
      val _1 = 10: @lzy
      println(_1)
      20: @lzy
      val _2 = 20: @lzy
      println(_2)
      30: @lzy
      val _3 = 30: @lzy
      println(_3)
      40: @lzy
      val _4 = 40: @lzy
      println(_4)
      50: @lzy
      val _5 = 50: @lzy
      println(_5)
      0
    }
  }

  test("simple 2 @") {
    inverse_macros.transform {
      println("simple 2")
      val _1 = defer {println(10); 10 }
      val _2 = defer {println(20); 20 }
      val _3 = defer {println(30); 30 }
      val _4 = defer {println(40); 40 }
      val _5 = defer {println(50); 50 }
      println(_5)
      println(_4)
      println(_3)
      println(_2)
      println(_1)
      0
    }
  }

  test("lazy @") {
//    inverse_macros.transform {
//      def m0() = {
//        shift((k: Int => Int) => k(k(7))) * 2
//      }
//
//      def m1() = {
//        2 * shift((k: Int => Int) => k(k(7)))
//      }
//      assertEquals(28, reset[Int, Int](m0()))
//      assertEquals(28, reset[Int, Int](m1()))
//
//      reset[Int, Int](10: Int@lzy)
//
//      {
//        val f = () => shift { k: (Int => Int) => k(7)}
//        val g: () => Int@cps[Int] = f
//
//        assertEquals(7, reset[Int, Int](g()))
//      }
//
//      {
//        val g: () => Int@cps[Int] = () => shift { k: (Int => Int) => k(7)}
//
//        assertEquals(7, reset[Int, Int](g()))
//      }
//
//      {
//        val g: () => Int@cps[Int] = () => 7
//
//        assertEquals(7, reset[Int, Int](g()))
//      }
//
//      {
//        val g: PartialFunction[Int, Int@cps[Int]] = {
//          case x => 7
//        }
//
//        assertEquals(7, reset[Int, Int](g(2)))
//      }
//
//      reset[Unit, Unit] {
//        var i = 0
//        ()
//      }
//      reset[Unit, Unit] {
//        var i = 0
//        1: @cpsParam[Unit, Unit]
//        ()
//      }
//      reset[Unit, Unit] {
//        var i = 0
//        1: @cpsParam[Unit, Unit]
//        1: @cpsParam[Unit, Unit]
//        ()
//      }
//    }
  }


  test("IfThenElse") {
//    inverse_macros.transform {
    //      val out = new StringBuilder
    //
    //            def printOut(x: Any): Unit = out ++= x.toString
    //
    //            {
    //              def test(x: Int) = {
    //                if (x <= 7)
    //                  shift { k: (Int => Int) => k(k(k(x)))}
    //                else
    //                  shift { k: (Int => Int) => k(x)}
    //              }
    //
    //              assert(10 == reset[Int, Int](1 + test(7)))
    //              assert(9 == reset[Int, Int](1 + test(8)))
    //            }
    //
    //            def test1(x: Int) = {
    //              if (x <= 7)
    //                shift { k: (Int => Int) => k(k(k(x)))}
    //              else
    //                x
    //            }
    //
    //            def test2(x: Int) = {
    //              if (x <= 7)
    //                x
    //              else
    //                shift { k: (Int => Int) => k(k(k(x)))}
    //            }
    //
    //            {
    //              assertEquals(10, reset[Int, Int](1 + test1(7)))
    //              assertEquals(9, reset[Int, Int](1 + test1(8)))
    //              assertEquals(8, reset[Int, Int](1 + test2(7)))
    //              assertEquals(11, reset[Int, Int](1 + test2(8)))
    //            }
    //
    //            def test3(x: Int) = {
    //              if (x <= 7)
    //                shift { k: (Unit => Unit) => printOut("abort")}
    //            }
    //
    //            {
    //              out.clear()
    //              printOut(reset[Unit, Unit] {
    //                test3(7);
    //                printOut("alive")
    //              })
    //              printOut(reset[Unit, Unit] {
    //                test3(8);
    //                printOut("alive")
    //              })
    //              assertEquals("abort()alive()", out.toString)
    //            }
    //
    //            def util(x: Boolean) = shift { k: (Boolean => Int) => k(x)}
    //
    //            def test4(x: Int) = {
    //              if (util(x <= 7))
    //                x - 1
    //              else
    //                x + 1
    //            }
    //
    //            {
    //              assertEquals(6, reset[Int, Int](test4(7)))
    //              assertEquals(9, reset[Int, Int](test4(8)))
    //            }
    //
    //            def sh(x1: Int) = shift((k: Int => Int) => k(k(k(x1))))
    //
    //
    //            def testA(x1: Int): Int@cps[Int] = {
    //              sh(x1)
    //              if (x1 == 42) x1 else sh(x1)
    //            }
    //
    //            def testB(x1: Int): Int@cps[Int] = {
    //              if (sh(x1) == 43) x1 else x1
    //            }
    //
    //            def testC(x1: Int): Int@cps[Int] = {
    //              sh(x1)
    //              if (sh(x1) == 44) x1 else x1
    //            }
    //
    //            def testD(x1: Int): Int@cps[Int] = {
    //              sh(x1)
    //              if (sh(x1) == 45) x1 else sh(x1)
    //            }
    //
    //            {
    //              assertEquals(10, reset[Int, Int](1 + testA(7)))
    //              assertEquals(10, reset[Int, Int](1 + testB(9)))
    //              assertEquals(10, reset[Int, Int](1 + testC(9)))
    //              assertEquals(10, reset[Int, Int](1 + testD(7)))
    //            }
//          }
  }
}
