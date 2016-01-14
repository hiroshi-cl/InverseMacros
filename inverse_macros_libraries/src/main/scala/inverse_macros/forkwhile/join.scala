package inverse_macros.forkwhile

import inverse_macros.{IMAnnotation, IMTransformer}

import scala.concurrent.Future
import scala.language.higherKinds

class join extends IMAnnotation

object join extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                        (api: c.internal.TypingTransformApi)
                        (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._

    val ccsym = symbolOf[ForkContext]
    val cpsym = symbolOf[fork]

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        val ex = TermName(c.freshName("join_ex_vd"))
        val joins = q"try $rhs catch {case $ex: $ccsym => $ex.list.foreach(_.sync) }"
        List(treeCopy.ValDef(head, mods, name, tpt, api.typecheck(joins))) -> cont

      case _ =>
        val ex = TermName(c.freshName("join_ex_"))
        val joins = q"try $head catch {case $ex: $ccsym => $ex.list.foreach(_.sync) }"
        List(api.typecheck(joins)) -> cont
    }
  }
}
