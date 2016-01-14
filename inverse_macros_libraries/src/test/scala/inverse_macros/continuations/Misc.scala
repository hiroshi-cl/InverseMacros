package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

// misc は type の関係で本家でもまともに動かないので削除
// T3233 など細かいやつを削除
// while は未実装なので除去

// 型推論が弱いので reset の type ascription, shift の引数の type ascription は省略できない (本家では適宜補ってくれる)
// HigherOrder ダメポ, Inference もダメポ (toplevel val は intercept しないの忘れてた…) <- 動くようになった
// Return も仕様にマッチしていないようでダメポ <- return 除去を入れたことで大丈夫になった
// trycatch0, T3233:  shift/reset に使っている Throwable でキャッチされる… <- 修正済み

// infinite loop
class Misc extends JUnitSuite  {

  def double[B](n: Int)(k: Int => B): B = k(n * 2)

  @Test def t2864 {
    reset[Unit,Unit] {
      val result1 = shift(double[Unit](100))
      val result2 = shift(double[Unit](result1))
      assertEquals(400, result2)
    }
  }

  def foo: Int @cps[Int] = {
    val a0 = shift((k: Int => Int) => k(0))
    val x0 = 2
    val a1 = shift((k: Int => Int) => x0)
    0
  }

  /*
  def bar: ControlContext[Int,Int,Int] = {
    shiftR((k:Int=>Int) => k(0)).flatMap { a0 =>
    val x0 = 2
    shiftR((k:Int=>Int) => x0).map { a1 =>
    0
    }}
  }
*/

  @Test def t2934 = {
    assertEquals(List(3, 4, 5), reset[List[Int], List[Int]] {
      val x = shift(List(1, 2, 3).flatMap[Int, List[Int]])
      List(x + 2)
    })
  }

  class Bla {
    val x = 8
    def y[T] = 9
  }

  /*
  def bla[A] = shift { k:(Bla=>A) => k(new Bla) }
*/

  def bla1 = shift { k: (Bla => Bla) => k(new Bla) }
  def bla2 = shift { k: (Bla => Int) => k(new Bla) }

  def fooA = bla2.x
  def fooB[T] = bla2.y[T]

  // TODO: check whether this also applies to a::shift { k => ... }

  @Test def t3225Mono(): Unit = {
    assertEquals(8, reset[Bla,Bla](bla1).x)
    assertEquals(8, reset[Int,Int](bla2.x))
//    assertEquals(9, reset(bla2.y[Int])) <- type ascription 追加
    assertEquals(9, reset[Int,Int](bla2.y[Int]))
//    assertEquals(9, reset(bla2.y)) <- type ascription 追加
    assertEquals(9, reset[Int,Int](bla2.y))
    assertEquals(8, reset[Int,Int](fooA))
    assertEquals(9, reset[Int,Int](fooB))
    0
  }

  def blaX[A] = shift { k: (Bla => A) => k(new Bla) }

  def fooX[A] = blaX[A].x
  def fooY[A] = blaX[A].y[A]

  @Test def t3225Poly(): Unit = {
    assertEquals(8, reset[Bla,Bla](blaX[Bla]).x)
    assertEquals(8, reset[Int,Int](blaX[Int].x))
    assertEquals(9, reset[Int,Int](blaX[Int].y[Int]))
    assertEquals(9, reset[Int,Int](blaX[Int].y))
    assertEquals(8, reset[Int,Int](fooX[Int]))
    assertEquals(9, reset[Int,Int](fooY[Int]))
    0
  }

  // TODO: termination を成功させる
  def capture(): Int @suspendable = 42

  @Test def t3501: Unit = reset[Unit,Unit] {
    var i = 0
    while (i < 5) {
      i += 1
      val y = capture()
      val s = y
      assertEquals(42, s)
    }
    assertEquals(5, i)
  }
}


