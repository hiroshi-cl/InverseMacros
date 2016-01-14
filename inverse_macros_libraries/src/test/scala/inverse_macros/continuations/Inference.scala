package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class Inference extends JUnitSuite  {

  //  object A {
  //    class foo[-B, +C] extends StaticAnnotation with TypeConstraint
  //
  //    def shift[A, B, C](fun: (A => B) => C): A @foo[B, C] = ???
  //    def reset[A, C](ctx: => (A @foo[A, C])): C = ???
  //
  //    def m1 = reset[Range,Range] { shift { f: (Int => Range) => f(5) }.to(10) }
  //  }

  object B {

    def m1 = reset[Range,Range] { shift { f: (Int => Range) => f(5) }.to(10) }
    def m2 = reset[Range,Range] { val a = shift { f: (Int => Range) => f(5) }; a.to(10) }

    val x1 = inverse_macros.transform(reset[Range,Range] {
      shift { cont: (Int => Range) =>
        cont(5)
      }.to(10)
    })

    val x2 = inverse_macros.transform(reset[Range,Range] {
      val a = shift { cont: (Int => Range) =>
        cont(5)
      }
      a.to(10)
    }) // x is now Range(5, 6, 7, 8, 9, 10)

    val x3 = inverse_macros.transform(reset[Int,Int] {
      shift { cont: (Int => Int) =>
        cont(5)
      } + 10
    }) // x is now 15

    val x4 = inverse_macros.transform(reset[List[Int],List[Int]] {
      10 :: shift { cont: (List[Int] => List[Int]) =>
        cont(List(1, 2, 3))
      }
    }) // x is List(10, 1, 2, 3)

    val x5 = inverse_macros.transform(reset[Range,Range] {
      new scala.runtime.RichInt(shift { cont: (Int => Range) =>
        cont(5)
      }) to 10
    })
  }

  @Test def implicit_infer_annotations = {
    import B._
    assertEquals(5 to 10, x1)
    assertEquals(5 to 10, x2)
    assertEquals(15, x3)
    assertEquals(List(10, 1, 2, 3), x4)
    assertEquals(5 to 10, x5)
  }

  def test(x: => Int @cpsParam[String, Int]) = 7

  def test2() = {
    val x = shift { k: (Int => String) => 9 }
    x
  }

  def test3(x: => Int @cpsParam[Int, Int]) = 7

  def util() = shift { k: (String => String) => "7" }

  @Test def infer1: Unit = {
    test { shift { k: (Int => String) => 9 } }
    test { shift { k: (Int => String) => 9 }; 2 }
    //    test { shift { k: (Int => String) => 9 }; util() }  <-- doesn't work
    test { shift { k: (Int => String) => 9 }; util(); 2 }

    test { shift { k: (Int => String) => 9 }; { test3(0); 2 } }

    test3 { { test3(0); 2 } }

  }

}
