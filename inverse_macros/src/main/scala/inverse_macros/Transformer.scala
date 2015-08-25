package inverse_macros

private[inverse_macros] final class Transformer[C <: scala.reflect.macros.blackbox.Context](val c: C) extends ANFTransformers[C] {

  import c.universe._
  import c.internal._

  private[inverse_macros] final val util = new Util[c.type](c)

  import util._

  private[inverse_macros] type Tr = PartialFunction[(Tree, TypingTransformApi), Tree]

  private[inverse_macros] def listTransform(list: List[Tree], api: TypingTransformApi): List[Tree] = list match {
    case Nil => Nil

    case last :: Nil =>
      if (detectAnnotatedTyped(last)) {
        anfTransform(last, api) match {
          case Nil =>
            Nil
          case newLast :: Nil =>
            newLast :: Nil // no annotation processing
          case newList =>
            listTransform(newList.map(api.typecheck), api)
        }
      } else
        last :: Nil

    case head :: tail =>
      if (detectAnnotatedTyped(head)) {
        anfTransform(head, api) match {
          case Nil =>
            tail

          case (newHead@ValDef(mods, name, tpt, rhs)) :: Nil =>
            if (getIMAnnotations(rhs.tpe).nonEmpty) {
              val (um, targs, argss) = getUserMacro(rhs.tpe)
              val (hs, ts) = um.transform(c)(targs, argss)(api)(newHead, tail).asInstanceOf[(List[Tree], List[Tree])]
              hs.map(api.typecheck) ++ listTransform(ts.map(api.typecheck), api) // 無限ループ回避のためにもリストは元より短く
            } else
              newHead :: listTransform(tail, api)

          case newHead :: Nil =>
            if (getIMAnnotations(newHead.tpe).nonEmpty) {
              val (um, targs, argss) = getUserMacro(newHead.tpe)
              val (hs, ts) = um.transform(c)(targs, argss)(api)(newHead, tail).asInstanceOf[(List[Tree], List[Tree])]
              hs.map(api.typecheck) ++ listTransform(ts.map(api.typecheck), api) // 無限ループ回避のためにもリストは元より短く
            } else
              newHead :: listTransform(tail, api)

          case newList =>
            listTransform(newList.map(api.typecheck) ++ tail, api)
        }
      } else
        head +: listTransform(tail, api)
  }

  private[inverse_macros] def blockTransform(tree: Tree, api: TypingTransformApi) =
    if (detectAnnotatedTyped(tree))
      tree match {
        case EmptyTree =>
          EmptyTree
        case Block(stats, expr) =>
          api.typecheck(q"{..${listTransform(stats :+ expr, api)}}")
        case _ =>
          api.typecheck(q"{..${listTransform(List(tree), api)}}")
      }
    else
      tree // do nothing

  def imTransform(tree: Tree) = c.typecheck(typingTransform(tree)(blockTransform))

  private[inverse_macros] def detectAnnotatedTyped(tree: Tree): Boolean =
    transform(tree) { (t, api) =>
      t match {
        case ValDef(mods, _, _, _)
          if mods.annotations.exists(_.tpe <:< typeOf[IMSynth]) || // for untyped trees
            t.symbol != null && t.symbol.annotations.exists(_.tree.tpe <:< typeOf[IMSynth]) => // for typed trees
          EmptyTree
        case _: ValDef => api.default(t)
        case _: LabelDef => api.default(t)
        case _: CaseDef => api.default(t)
        case _: MemberDef => EmptyTree
        case _: Ident if t.symbol != null && t.symbol.isTerm && t.symbol.asTerm.isByNameParam =>
          api.default(t)
        case _: Ident => EmptyTree
        case _ => api.default(t)
      }
    } exists {
      _.tpe match {
        case AnnotatedType(annotations, underlying) =>
          annotations.exists(_.tree.tpe <:< typeOf[IMAnnotation])
        case _ => false
      }
    }

}
