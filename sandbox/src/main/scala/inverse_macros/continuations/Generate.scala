package inverse_macros.continuations

object Generate {

  def strings(head: String, mid: String, tail: String)(st: Int, en: Int) = {
    for (rep <- st to en)
      println(head + (mid * rep) + tail)
  }

  def main(args: Array[String]): Unit = {
    strings("echo \"package inverse_macros.continuations; object Main { def main(args: Array[String]): Unit = reset[Unit, Unit]{ val a = Array(0); ", "a(0) += shift((k: Int => Unit) => k(1)); ", "(); }}\" > Main.scala")(31, 50)
  }
}
