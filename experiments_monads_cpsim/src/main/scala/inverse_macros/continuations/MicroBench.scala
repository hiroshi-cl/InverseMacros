package inverse_macros.continuations

import bench.BenchMacro
import scala.language.implicitConversions

object MicroBench {
  final val S = 10
  final val R = 100000//0
  final val MAX = 50 //50

  def simpleBench(body: => Unit): Double = {
    System.gc()
    Thread.sleep(100)
    // 捨てる分も同じように取る
    val times = for (i <- 1 to 2 * S) yield {
      val st = System.nanoTime()
      var i = 0
      while (i < R) {
        body
        i += 1
      }
      val time = (System.nanoTime() - st) * 1e-9
      System.gc()
      Thread.sleep(100)
      time
    }
    System.gc()
    Thread.sleep(100)
    // ここで捨てる
    val time = times.drop(S).sum / S
    //    printf("%.6f\t%s\n", time, times.drop(S))
    printf("%.6f\n", time)
    time
  }

  def strings(head: String, mid: String, tail: String)(st: Int, en: Int) = {
    for (rep <- st to en)
      println(head + (mid * rep) + tail)
  }

  import Monads._

  val a = Array[Int](0)
  val r_option = {
    import OptionMonad._
    inverse_macros.transform {
      BenchMacro.macroList2[Double](
        "simpleBench(Monads.reify[Unit, Option[Unit]]{ ", "a(0) += Some(1).reflect; ", "a(0) += None.asInstanceOf[Option[Int]].reflect; ", "(); })")(1, MAX)
    }
  }
  val r_state = {
    import StateMonad._
    inverse_macros.transform {
      BenchMacro.macroList[Double](
        "simpleBench(Monads.reify[Unit, Int => (Int, Unit)]{ ", "a(0) += get[Int]; put(a(0)); ", "(); }.apply(0))")(1, MAX)
    }
  }

  def main(args: Array[String]): Unit = {
    println("n\toption\tstate")
    for (i <- 0 until MAX)
      printf("%d\t%.6f\t%.6f\n", i + 1, r_option(i), r_state(i))
  }
}
