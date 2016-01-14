package inverse_macros.skipto

import inverse_macros.{IMAnnotation, IMTransformer}

class label extends IMAnnotation

object label extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                               (api: c.internal.TypingTransformApi)
                               (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._

    val lbsym = symbolOf[label]
    val apsym = lbsym.companion.asModule.typeSignature.member(TermName("apply"))
    val casym = lbsym.companion.asModule.typeSignature.member(TermName("cache"))

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        c.abort(head.pos,  show(head) + ": you cannot bind labels to variables.")

      case _ =>
        val q"$apsym(${name: String})" = head
        val contBlock = api.recur(api.typecheck(q"{..$cont}"))
        List(api.typecheck(q"$casym($name){$contBlock}")) -> cont
    }
  }

  def apply(name: String): Unit@label = ???

  def cache(name: String)(block: => Any): Unit = {}
}
