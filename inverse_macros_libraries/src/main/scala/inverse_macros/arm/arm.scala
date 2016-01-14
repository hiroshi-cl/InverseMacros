package inverse_macros.arm

import inverse_macros.{IMAnnotation, IMTransformer}

class arm extends IMAnnotation

object arm extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                               (api: c.internal.TypingTransformApi)
                               (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._

    val lzsym = symbolOf[arm]

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        val transformed = api.recur(api.typecheck(q"{..$cont}"))
        val tryFinally = api.typecheck(q"try {$transformed} finally { ${head.symbol}.close }")
        List(head, tryFinally) -> Nil

      case _ =>
        c.warning(head.pos, show(head) + " is not used. (not binded) Is it expected?")
        List(head) -> cont
    }
  }

  def apply[T](r: T): T@arm = r
}
