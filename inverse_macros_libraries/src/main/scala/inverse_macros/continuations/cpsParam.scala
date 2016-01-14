package inverse_macros.continuations

import inverse_macros.{IMTransformer, IMAnnotation}

class cpsParam[-B, +C] extends IMAnnotation

object cpsParam extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                               (api: c.internal.TypingTransformApi)
                               (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.internal._
    import c.universe._
    import Flag._

    val ccsym = symbolOf[ControlContext[_, _, _]]
    val cpsym = symbolOf[cpsParam[_, _]]

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        c.typecheck(q"@inline $SYNTHETIC def ${TermName(c.freshName(name + ""))} (${TermName(c.freshName(name + ""))} : $tpt) : ${TypeTree(cont.last.tpe)} = ???") match {
          case ddef@DefDef(_, _, _, List(List(param)), _, _) =>
            val paramRef = gen.mkAttributedRef(param.symbol)
            val newHead = treeCopy.ValDef(head, mods, name, tpt, paramRef)
            val newRhs = api.recur(api.typecheck(q"{$newHead; ..$cont}"))
            c.internal.setOwner(ddef.symbol, api.currentOwner)
            // change owners of local variables
            c.internal.changeOwner(rhs, head.symbol, api.currentOwner)
            c.internal.changeOwner(newRhs, api.currentOwner, ddef.symbol)
             // build new defDef
            val func = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, TypeTree(newRhs.tpe), newRhs)
            setInfo(func.symbol, methodType(List(param.symbol), newRhs.tpe))

            val ex = TermName(c.freshName(ddef.name + "$" + "ex")) // workaround for a bug(?) in 2.11.7
            val tp = tq"$ccsym[${rhs.tpe.asInstanceOf[AnnotatedType].underlying}, ..$targs]"
            val ctlBlock = newRhs.tpe match {
              case AnnotatedType(annotations, underlying)
                if annotations.exists(_.tree.tpe.typeConstructor <:< typeOf[cpsParam[_, _]].typeConstructor) =>
                val tt = api.typecheck(q"{val $ex: $tp = ???; $ex.flatMap(${func.symbol})}").tpe.typeArgs
                val ctp = c.typecheck(tq"${tt.head}@$cpsym[..${tt.tail}]", mode = c.TYPEmode)
                q"${func.symbol} (try {$rhs} catch { case $ex: $ccsym[_,_,_] => throw $ex.asInstanceOf[$tp].flatMap(${func.symbol}) }) : $ctp"
              case _ =>
                val tt = api.typecheck(q"{val $ex: $tp = ???; $ex.map(${func.symbol})}").tpe.typeArgs
                val ctp = c.typecheck(tq"${tt.head}@$cpsym[..${tt.tail}]", mode = c.TYPEmode)
                q"${func.symbol} (try {$rhs} catch { case $ex: $ccsym[_,_,_] => throw $ex.asInstanceOf[$tp].map(${func.symbol}) }): $ctp"
            }

            List(func, c.internal.changeOwner(c.typecheck(ctlBlock), c.internal.enclosingOwner, api.currentOwner)) -> Nil
        }

      case _ =>
        c.typecheck(q"@inline $SYNTHETIC def ${TermName(c.freshName("unit"))} () : ${TypeTree(cont.last.tpe)} = ???") match {
          case ddef@DefDef(_, _, _, _, _, _) =>
            val newRhs = api.recur(api.typecheck(q"{..$cont}"))
            c.internal.setOwner(ddef.symbol, api.currentOwner)
            // change owners of local variables
            c.internal.changeOwner(newRhs, api.currentOwner, ddef.symbol)
            // build new defDef
            val f = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, TypeTree(newRhs.tpe), newRhs)
            setInfo(f.symbol, methodType(Nil, newRhs.tpe))
            val func = c.typecheck(f)

            val ex = TermName(c.freshName(ddef.name + "$" + "ex")) // workaround for a bug(?) in 2.11.7
            val tp = c.typecheck(tq"$ccsym[${definitions.AnyTpe}, ..$targs]", mode = c.TYPEmode)
            val ctlBlocks = newRhs.tpe match {
              case AnnotatedType(annotations, underlying)
                if annotations.exists(_.tree.tpe.typeConstructor <:< typeOf[cpsParam[_, _]].typeConstructor) =>
                val tt = api.typecheck(q"{val $ex: $tp = ???; $ex.flatMap(_ => ${func.symbol})}").tpe.typeArgs
                val ctp = c.typecheck(tq"${tt.head}@$cpsym[..${tt.tail}]", mode = c.TYPEmode)
                List(
                  q"try {$head} catch { case $ex: $ccsym[_,_,_] => throw $ex.asInstanceOf[$tp].flatMap(_ => ${func.symbol}) }",
                  q"${func.symbol} : $ctp"
                )
              case _ =>
                val tt = api.typecheck(q"{val $ex: $tp = ???; $ex.map(_ => ${func.symbol})}").tpe.typeArgs
                val ctp = tq"${tt.head}@$cpsym[..${tt.tail}]"
                List(
                  q"try {$head} catch { case $ex: $ccsym[_,_,_] => throw $ex.asInstanceOf[$tp].map(_ => ${func.symbol}) }",
                  q"${func.symbol} : $ctp"
                )
            }

            (func +: ctlBlocks.map(t => c.internal.changeOwner(c.typecheck(t), c.internal.enclosingOwner, api.currentOwner))) -> Nil
        }
    }
  }
}
