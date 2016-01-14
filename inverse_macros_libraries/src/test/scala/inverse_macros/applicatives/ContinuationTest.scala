package inverse_macros.applicatives

import inverse_macros.applicatives.Applicatives._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class ContinuationTest extends FunSuite {

  test("simple 1 @") {
    import StateApplicative._
    inverse_macros.transform {
      10: @applicative[Int => (Int, Int)]
      val _1 = 10: @applicative[Int => (Int, Int)]
      println(_1)
      20: @applicative[Int => (Int, Int)]
      val _2 = 20: @applicative[Int => (Int, Int)]
      println(_2)
      30: @applicative[Int => (Int, Int)]
      val _3 = 30: @applicative[Int => (Int, Int)]
      println(_3)
      40: @applicative[Int => (Int, Int)]
      val _4 = 40: @applicative[Int => (Int, Int)]
      println(_4)
      50: @applicative[Int => (Int, Int)]
      val _5 = 50: @applicative[Int => (Int, Int)]
      println(_5)
      0
    }
  }

  test("simple 2 @") {
    import StateApplicative._
    inverse_macros.transform {
      val _1 = 10: @applicative[Int => (Int, Int)]
      10: @applicative[Int => (Int, Int)]
      println(_1)
      val _2 = 20: @applicative[Int => (Int, Int)]
      20: @applicative[Int => (Int, Int)]
      println(_2)
      val _3 = 30: @applicative[Int => (Int, Int)]
      30: @applicative[Int => (Int, Int)]
      println(_3)
      val _4 = 40: @applicative[Int => (Int, Int)]
      40: @applicative[Int => (Int, Int)]
      println(_4)
      val _5 = 50: @applicative[Int => (Int, Int)]
      50: @applicative[Int => (Int, Int)]
      println(_5)
      0
    }
  }


  test("reader") {
    import StateApplicative._
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
      assert {
        reify[Int, Int => (Int, Int)] {
          val x = get[Int] + get[Int] + 1
          val y = 1 + get[Int]
          x * y
        }.apply(10)._2 == 231
      }
      assert {
        reify[Int, Int => (Int, Int)] {
          val x = get[Int] + get[Int] + 1
          val y = 1 + get[Int]
          println(x + y)
          val z = x * y
          println(z)
          z
        }.apply(10)._2 == 231
      }
    }
  }

  test("writer") {
    import StateApplicative._
    inverse_macros.transform {
      assert {
        reify[Unit, Int => (Int, Unit)](put(1)).apply(10)._1 == 1
      }
      assert {
        reify[Int, Int => (Int, Int)] {
          put(1)
          ((x: Int) => (x, x + 1)).reflect + 1
        }.apply(10) ==(10, 12)
      }
    }
  }

}