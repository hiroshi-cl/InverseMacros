package inverse_macros.concurrent

import inverse_macros.{IMAnnotation, IMTransformer}

class lzy[+MA] extends IMAnnotation

object lzy extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                               (api: c.internal.TypingTransformApi)
                               (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._

    val lcsym = symbolOf[ConcurrentContext[_]]
    val lzsym = symbolOf[lzy[_]]


    def dval(tpe: Type): Tree = c.typecheck(tpe match {
      case _ if tpe weak_<:< weakTypeOf[Unit] => q"() : $tpe"
      case _ if tpe weak_<:< weakTypeOf[Boolean] => q"false : $tpe"
      case _ if tpe weak_<:< weakTypeOf[Char] => q"'\0' : $tpe"
      case _ if tpe weak_<:< weakTypeOf[Byte] => q"${0.toByte} : $tpe"
      case _ if tpe weak_<:< weakTypeOf[Short] => q"${0.toByte} : $tpe"
      case _ if tpe weak_<:< weakTypeOf[Int] => q"0 : $tpe"
      case _ if tpe weak_<:< weakTypeOf[Long] => q"0l : $tpe"
      case _ if tpe weak_<:< weakTypeOf[Float] => q"0.0f : $tpe"
      case _ if tpe weak_<:< weakTypeOf[Double] => q"0.0d : $tpe"
      case _ => q"null : $tpe"
    })

    val mtp = targs.head

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        val AnnotatedType(_, underlying) = rhs.tpe
        val intmNme = c.freshName(name.toString)
        val intmVar = c.typecheck(q"var ${TermName(intmNme + "$var")} : $mtp = null")
        c.internal.setOwner(intmVar.symbol, api.currentOwner)
        val newRhs =
          c.typecheck(q"try { $rhs : $underlying } catch { case c: $lcsym[_] => ${intmVar.symbol} = c.m.asInstanceOf[$mtp]; ${dval(underlying)} }")
        val newVal = treeCopy.ValDef(head, mods, name, tpt, newRhs)
        c.internal.changeOwner(newRhs, c.internal.enclosingOwner, head.symbol)
        c.internal.changeOwner(newRhs, api.currentOwner, head.symbol)
        val intmVal = c.typecheck(q"val ${TermName(intmNme + "$val")} : $mtp = ${intmVar.symbol}")
        c.internal.setOwner(intmVal.symbol, api.currentOwner)

        // traverse
        val newCont = for (t <- cont) yield
          c.internal.transform(t) {
            (tree, api) => tree match {
              case i@Ident(_) if i.symbol == head.symbol =>
                c.typecheck(q"if (${intmVal.symbol} == null) ${head.symbol} else ${intmVal.symbol}.jget()")
              case _ =>
                api.default(tree)
            }
          }


        List(intmVar, newVal, intmVal) -> newCont

      case _ =>
        c.warning(head.pos, show(head) + " is not used. (not binded) Is it expected?")
        List(head) -> cont
    }
  }

}
