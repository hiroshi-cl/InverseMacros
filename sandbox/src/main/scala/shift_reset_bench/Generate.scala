package shift_reset_bench



object Generate {
  def simpleBench(body: => Unit): Double = {
    val st = System.nanoTime()
    var i = 0
    while (i < 1000000) {
      body
      i += 1
    }
    val time = (System.nanoTime() - st) * 1e-9
    println(time)
    time
  }

  def strings(head: String, mid: String, tail: String)(st: Int, en: Int) = {
    for (rep <- st to en)
      println(head + (mid * rep) + tail)
  }

  def main(args: Array[String]): Unit = {
    val a = Array[Int](0)
    strings("simpleBench(reset[Unit, Unit]{ ", "a(0) += 1; ", "(); }),")(1, 100)
    strings("simpleBench(reset[Unit, Unit]{ ", "a(0) += shift((k: Int => Unit) => k(1)); ", "(); }),")(1, 100)
    strings("simpleBench(reset[Unit, Unit]{ ", "a(0) += (1 : @cpsParam[Unit, Unit]); ", "(); }),")(1, 100)
  }
}
