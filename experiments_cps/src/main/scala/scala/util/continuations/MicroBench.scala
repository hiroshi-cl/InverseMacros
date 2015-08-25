package scala.util.continuations

import bench.BenchMacro

object MicroBench {
  final val S = 10
  final val R = 1000000
  final val MAX = 50

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

  def main(args: Array[String]): Unit = {
    val a = Array[Int](0)
    val r_pure = BenchMacro.macroList[Double](
      "simpleBench(reset[Unit, Unit]{ ", "a(0) += 1; ", "(); })")(1, MAX)
    val r_shift = BenchMacro.macroList[Double](
      "simpleBench(reset[Unit, Unit]{ ", "a(0) += shift((k: Int => Unit) => k(1)); ", "(); })")(1, MAX)
    val r_lift = BenchMacro.macroList[Double](
      "simpleBench(reset[Unit, Unit]{ ", "a(0) += (1 : @cpsParam[Unit, Unit]); ", "(); })")(1, MAX)

    println("n\tpure\tshift\tlift")
    for(i <- 0 until MAX)
      printf("%d\t%.6f\t%.6f\t%.6f\n", i + 1, r_pure(i), r_shift(i), r_lift(i))
  }
}
