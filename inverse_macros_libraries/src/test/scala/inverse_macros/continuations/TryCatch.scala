package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class TryCatch extends JUnitSuite  {

  def foo = try {
    shift((k: Int => Int) => k(7))
  } catch {
    // shift/reset に使っている ControlThrowable にキャッチされる…
    case ex: Throwable if !ex.isInstanceOf[ControlThrowable] =>
      9
  }

  def bar = try {
    7
  } catch {
    case ex: Throwable =>
      shiftUnit0[Int, Int](9)
  }

  @Test def trycatch0 = {
    assertEquals(10, reset[Int,Int] { foo + 3 })
    assertEquals(10, reset[Int,Int] { bar + 3 })
  }

  def fatal: Int = throw new Exception()

  def foo1 = try {
    fatal
    shift((k: Int => Int) => k(7))
  } catch {
    case ex: Throwable =>
      9
  }

  def foo2 = try {
    shift((k: Int => Int) => k(7))
    fatal
  } catch {
    case ex: Throwable =>
      9
  }

  def bar1 = try {
    fatal
    7
  } catch {
    case ex: Throwable =>
      shiftUnit0[Int, Int](9) // regular shift causes no-symbol doesn't have owner
  }

  def bar2 = try {
    7
    fatal
  } catch {
    case ex: Throwable =>
      shiftUnit0[Int, Int](9) // regular shift causes no-symbol doesn't have owner
  }

  @Test def trycatch1 = {
    assertEquals(12, reset[Int,Int] { foo1 + 3 })
    assertEquals(12, reset[Int,Int] { foo2 + 3 })
    assertEquals(12, reset[Int,Int] { bar1 + 3 })
    assertEquals(12, reset[Int,Int] { bar2 + 3 })
  }

  trait AbstractResource[+R <: AnyRef] {
    def reflect[B]: R @cpsParam[B, Either[Throwable, B]] = shift[R, B, Either[Throwable, B]](acquireFor)
    def acquireFor[B](f: R => B): Either[Throwable, B] = {
      import scala.util.control.Exception._
      catching(List(classOf[Throwable]): _*) either (f(null.asInstanceOf[R]))
    }
  }

  @Test def t3199 = {
    val x = new AbstractResource[String] {}
    val result = x.acquireFor(x => 7)
    assertEquals(Right(7), result)
  }

}

object AvoidClassT3233 extends JUnitSuite  {
  def foo(x: Int) = {
    try {
      throw new Exception
      shiftUnit0[Int, Int](7)
    } catch {
      // shift/reset に使っている ControlThrowable にキャッチされる…
      case ex: Throwable if !ex.isInstanceOf[ControlThrowable] =>
        val g = (a: Int) => a
        9
    }
  }
}

class T3233 extends JUnitSuite  {
  // work around scalac bug: Trying to access the this of another class: tree.symbol = class TryCatch, ctx.clazz.symbol = <$anon: Function1>
  import AvoidClassT3233._
  @Test def t3223 = {
    assertEquals(9, reset[Int,Int](foo(0)))
  }
}