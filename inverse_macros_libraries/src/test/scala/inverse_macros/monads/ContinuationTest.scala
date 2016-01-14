package inverse_macros.monads

import debug._
import org.junit.Assert._
import org.scalatest.FunSuite

import scala.language.implicitConversions
import Monads._

class ContinuationTest extends FunSuite {
  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  test("simple 1 @") {
    import OptionMonad._
    inverse_macros.transform {
      10: @monad[Option[Int]]
      val _1 = 10: @monad[Option[Int]]
      println(_1)
      20: @monad[Option[Int]]
      val _2 = 20: @monad[Option[Int]]
      println(_2)
      30: @monad[Option[Int]]
      val _3 = 30: @monad[Option[Int]]
      println(_3)
      40: @monad[Option[Int]]
      val _4 = 40: @monad[Option[Int]]
      println(_4)
      50: @monad[Option[Int]]
      val _5 = 50: @monad[Option[Int]]
      println(_5)
      0
    }
  }

  test("simple 2 @") {
    import OptionMonad._
    inverse_macros.transform {
      val _1 = 10: @monad[Option[Int]]
      10: @monad[Option[Int]]
      println(_1)
      val _2 = 20: @monad[Option[Int]]
      20: @monad[Option[Int]]
      println(_2)
      val _3 = 30: @monad[Option[Int]]
      30: @monad[Option[Int]]
      println(_3)
      val _4 = 40: @monad[Option[Int]]
      40: @monad[Option[Int]]
      println(_4)
      val _5 = 50: @monad[Option[Int]]
      50: @monad[Option[Int]]
      println(_5)
      0
    }
  }


  test("simple unit @") {
    import OptionMonad._
    inverse_macros.transform {
      (): @monad[Option[Unit]]
      (): @monad[Option[Unit]]
      0
    }
    inverse_macros.transform {
      val a = Array[Int](1)
      reify[Unit, Option[Unit]] {
        a(0) += Some(1).reflect
        a(0) += None.asInstanceOf[Option[Int]].reflect
        ()
      }
    }
  }

  test("option") {
    import OptionMonad._
    inverse_macros.transform {
      assert(reify[Int, Option[Int]](10: @monad[Option[Int]]) == Some(10))
      assert(reify[Int, Option[Int]](Some(19).reflect + 1) == Some(20))
      assert(reify[Int, Option[Int]](None.asInstanceOf[Option[Int]].reflect).isEmpty)
    }
  }


  test("reader") {
    import StateMonad._
    inverse_macros.transform {
      assert {
        reify[Int, Int => (Int, Int)](1).apply(10)._2 == 1
      }
      assert {
        reify[Int, Int => (Int, Int)](get[Int] + 1).apply(10)._2 == 11
      }
      assert {
        reify[Int, Int => (Int, Int)](get[Int] + get[Int] + 1).apply(10)._2 == 21
      }
      assert {
        reify[Int, Int => (Int, Int)](((x: Int) => (x, x + 1)).reflect).apply(10)._2 == 11
      }
      assert {
        reify[Int, Int => (Int, Int)](((x: Int) => (x, x + 1)).reflect + 1).apply(10)._2 == 12
      }
    }
  }

  test("writer") {
    import StateMonad._
    inverse_macros.transform {
      assert {
        reify[Int, Int => (Int, Int)](10).apply(0)._2 == 10
      }
      assert {
        reify[Int, Int => (Int, Int)] {
          val x = 10
          ((_: Int) => (x, x)).reflect
          ((_: Int) => (20, x)).reflect
        }.apply(0)._1 == 20
      }
      assert {
        reify[Unit, Int => (Int, Unit)] {
          put(10)
        }.apply(0)._1 == 10
      }
      assert {
        reify[Unit, Int => (Int, Unit)] {
          put(10)
          put(20)
        }.apply(0)._1 == 20
      }
    }
  }
}
