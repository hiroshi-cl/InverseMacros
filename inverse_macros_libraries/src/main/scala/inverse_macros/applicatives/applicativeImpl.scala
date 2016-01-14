package inverse_macros.applicatives

import inverse_macros.{IMAnnotation, IMTransformer}

class applicativeImpl extends IMAnnotation

object applicativeImpl extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                        (api: c.internal.TypingTransformApi)
                        (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._

    val lcsym = symbolOf[ApplicativeContext[_]]
    val apsym = symbolOf[applicativeImpl].companion.asModule.typeSignature.member(TermName("apply"))
    val ausym = symbolOf[AUnit[_, _]]

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        def makeFunc(head: Tree, cont: List[Tree]): (Tree, List[Tree], List[Tree]) = head match {
          case ValDef(mods, name, tpt, q"$apsym($a, $ma)") =>
            val (rfunc, rcont, rmas) = makeFunc(cont.head, cont.tail)
            c.internal.resetFlag(head.symbol, Flag.PARAM)
            val func = c.typecheck(q"(${treeCopy.ValDef(head, Modifiers(Flag.PARAM), name, tpt, EmptyTree)}) => $rfunc")
            c.internal.changeOwner(rfunc, c.internal.enclosingOwner, func.symbol)
            c.internal.setOwner(head.symbol, func.symbol)
            (func, rcont, ma :: rmas)
          case ValDef(_, _, _, rhs) =>
            (rhs, head :: cont, Nil)
          case _ =>
            (head, head :: cont, Nil)
        }

        val (func, h :: newCont, r :: mas) = makeFunc(head, cont)
        val funcall = api.typecheck(mas.foldLeft(api.typecheck(q"$r.map($func)"))((f, a) => api.typecheck(q"$a.ap($f)")))
        h match {
          case ValDef(mods, name, tpt, rhs) =>
            val newTpe = c.typecheck(tq"${TypeTree(tpt.tpe)}@applicative[${funcall.tpe}]", mode = c.TYPEmode).tpe
            val newRhs = api.typecheck(q"(throw new $lcsym($funcall)) : $newTpe")
            c.internal.setOwner(h.symbol, api.currentOwner)
            Nil -> (treeCopy.ValDef(h, mods, name, TypeTree(newRhs.tpe), newRhs) :: newCont)
          case _ =>
            c.internal.setOwner(func.symbol, api.currentOwner)
            val newTpe = c.typecheck(tq"${TypeTree(tpt.tpe)}@applicative[${funcall.tpe}]", mode = c.TYPEmode).tpe
            val newHead = api.typecheck(q"(throw new $lcsym($funcall)) : $newTpe")
            Nil -> (newHead :: newCont)
        }


      case _ =>
        c.abort(head.pos, show(head) + ": @applicativeImpl is only for internal uses.")
    }
  }

  def apply[A, MA](a: A, ma: MA): A@applicativeImpl = ???
}
