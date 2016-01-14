package inverse_macros.concurrent

import scala.language.implicitConversions

object MicroBench {
  final val RUNS = 10
  final val CYCLE = 100
  final val SLEEP = 100
  final val ABORT = 30.0

  def simpleBench(body: => Unit): String = {
    System.gc()
    Thread.sleep(SLEEP)
    // 捨てる分も同じように取る
    val times = for (i <- 1 to 2 * RUNS) yield {
      val st = System.nanoTime()
      var i = 0
      while (i < CYCLE) {
        body
        i += 1
      }
      val time = (System.nanoTime() - st) * 1e-9
      if (time > ABORT) {
        println("ABORT")
        return "?"
      }
      System.gc()
      Thread.sleep(SLEEP)
      time
    }
    System.gc()
    Thread.sleep(SLEEP)
    // ここで捨てる
    val timeinv = RUNS / times.drop(RUNS).sum
    printf("%.6f\n", timeinv)
    "%.6f".format(timeinv)
  }

  def sfib(n: Int): Int = if (n > 1) sfib(n - 1) + sfib(n - 2) else 1

  def strib(n: Int): Int = if (n > 2) strib(n - 1) + strib(n - 2) + strib(n - 3) else if (n == 2) 2 else 1

  def main(args: Array[String]): Unit = {
    {
      val FibN = 35
      val t = simpleBench(sfib(FibN))
      val c = List.tabulate(FibN - 9)(i => simpleBench(FibCPS.runFib(FibN, i + 10)))
      val p = List.tabulate(FibN - 9)(i => simpleBench(FibCPSTrans.runFib(FibN, i + 10)))
      val m = List.tabulate(FibN - 9)(i => simpleBench(Fib.runFib(FibN, i + 10)))
      val a = List.tabulate(FibN - 9)(i => simpleBench(inverse_macros.applicatives.Fib.runFib(FibN, i + 10)))
      val s = List.tabulate(FibN - 19)(i => simpleBench(FibSync.runFib(FibN, i + 20)))
      println("fib_seq:\t" + t)
      println("n\tbeta\tmonad\tm_f/j\tapplicative")
      for (i <- 10 to FibN)
        printf("%d\t%s\t%s\t%s\t%s\n", i, c(i - 10), p(i - 10), m(i - 10), a(i - 10))
      println("n\tsync")
      for (i <- 20 to FibN)
        printf("%d\t%s\n", i, s(i - 20))
    }

    {
      val TribN = 25
      val t = simpleBench(strib(TribN))
      val c = List.tabulate(TribN - 4)(i => simpleBench(FibCPS.runTrib(TribN, i + 5)))
      val p = List.tabulate(TribN - 4)(i => simpleBench(FibCPSTrans.runTrib(TribN, i + 5)))
      val m = List.tabulate(TribN - 4)(i => simpleBench(Fib.runTrib(TribN, i + 5)))
      val a = List.tabulate(TribN - 4)(i => simpleBench(inverse_macros.applicatives.Fib.runTrib(TribN, i + 5)))
      val s = List.tabulate(TribN - 14)(i => simpleBench(FibSync.runTrib(TribN, i + 15)))
      println("trib_seq:\t" + t)
      println("n\tbeta\tmonad\tm_f/j\tapplicative")
      for (i <- 5 to TribN)
        printf("%d\t%s\t%s\t%s\t%s\n", i, c(i - 5), p(i - 5), m(i - 5), a(i - 5))
      println("n\tsync")
      for (i <- 15 to TribN)
        printf("%d\t%s\n", i, s(i - 15))
    }
  }
}
