package bench

import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object BenchMacro {

  private[this] class Bundle(val c: Context) {

    import c.universe._

    def transform(code: Tree): Tree = c.parse(code.asInstanceOf[Literal].value.value.asInstanceOf[String])

    def macroList[T: WeakTypeTag](head: Tree, mid: Tree, tail: Tree)(st: Tree, en: Tree) = {
      val s_head = head.asInstanceOf[Literal].value.value.asInstanceOf[String]
      val s_mid = mid.asInstanceOf[Literal].value.value.asInstanceOf[String]
      val s_tail = tail.asInstanceOf[Literal].value.value.asInstanceOf[String]
      val i_st = st.asInstanceOf[Literal].value.value.asInstanceOf[Int]
      val i_en = en.asInstanceOf[Literal].value.value.asInstanceOf[Int]
      val list = for (rep <- i_st to i_en) yield c.parse(s_head + (s_mid * rep) + s_tail)
      q"List.apply[${weakTypeOf[T]}](..$list)"
    }
  }

  def compile[T](code: String): T = macro Bundle.transform

  def macroList[T](head: String, mid: String, tail: String)(st: Int, en: Int): List[T] = macro Bundle.macroList[T]
}
