package inverse_macros.applicatives

import inverse_macros.{IMAnnotation, IMTransformer}

class applicative[+MA] extends IMAnnotation

object applicative extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                        (api: c.internal.TypingTransformApi)
                        (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._

    val lcsym = symbolOf[ApplicativeContext[_]]
    val lzsym = symbolOf[applicative[_]]
    val apsym = symbolOf[applicativeImpl].companion.asModule.typeSignature.member(TermName("apply"))
    val ausym = symbolOf[AUnit[_, _]]


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

    val atp = targs.head

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        val AnnotatedType(_, underlying) = rhs.tpe
        val newTpe = underlying.widen
        val intmNme = c.freshName(name.toString)
        val v = TermName(c.freshName(name + "$" + "aunit"))
        val ex = TermName(c.freshName(name + "$" + "ex"))
        val au = c.inferImplicitValue(c.typecheck(tq"$ausym[$newTpe,$atp]", mode = c.TYPEmode).tpe)
        val newVal = treeCopy.ValDef(head, mods, name, TypeTree(newTpe), c.typecheck(q"${dval(newTpe)}"))
        val newRhs = c.typecheck(q"try { val $v = $rhs;  $au.unit($v) } catch { case $ex: $lcsym[_] => $ex.m.asInstanceOf[$atp] }")
        val intmVal = c.typecheck(q"val ${TermName(intmNme + "$val")} : $atp = $newRhs")
        c.internal.setOwner(intmVal.symbol, api.currentOwner)
        c.internal.changeOwner(newRhs, head.symbol, intmVal.symbol)
        c.internal.changeOwner(newRhs, c.internal.enclosingOwner, intmVal.symbol)

        // traverse
        val newCont = for (t <- cont) yield
          c.internal.transform(t) {
            (tree, api) => tree match {
              case i@Ident(_) if i.symbol == head.symbol =>
                c.typecheck(q"$apsym(${head.symbol}, ${intmVal.symbol})")
              case _ =>
                api.default(tree)
            }
          }

        newCont.last match {
          case q"$apsym(${a: Tree}, ${ma: Tree})" =>
            val x = TermName(c.freshName(name + "$ap"))
            val vDef = c.typecheck(q"val $x: ${a.tpe} = ${newCont.last}")
            c.internal.setOwner(vDef.symbol, api.currentOwner)
            c.internal.changeOwner(newCont.last, api.currentOwner, vDef.symbol)
            val ref = c.internal.gen.mkAttributedRef(vDef.symbol)
            List(newVal, intmVal) -> (newCont.init :+ vDef :+ ref)
          case _ =>
            List(newVal, intmVal) -> newCont
        }

      case _ =>
        c.warning(head.pos, show(head) + " is not used. (not binded) Is it expected?")
        val ex = TermName(c.freshName("unit$" + "ex"))
        List(c.typecheck(q"try { $head } catch { case _: $lcsym[_] => }")) -> cont
    }
  }

}
