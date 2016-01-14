package inverse_macros.skipto

import inverse_macros.{IMAnnotation, IMTransformer}

class skipto extends IMAnnotation

object skipto extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                        (api: c.internal.TypingTransformApi)
                        (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._

    val sksym = symbolOf[skipto]
    val apsym = sksym.companion.asModule.typeSignature.member(TermName("apply"))
    val casym = symbolOf[label].companion.asModule.typeSignature.member(TermName("cache"))

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        c.abort(head.pos, show(head) + ": you cannot bind skipto operators to variables.")

      case _ =>
        val q"$apsym(${name: String})" = head
        val contBlock = api.recur(api.typecheck(q"{..$cont}"))

        // traverse
        val destBlocks = contBlock.find { case q"$casym(${dname: String}){$_}" => dname == name; case _ => false }
        if(destBlocks.isEmpty)
          c.abort(head.pos, show(head) + ": no skip destination.")
        val q"$casym($_){$cache}" = destBlocks.get


        List(api.typecheck(q"{$cache}")) -> Nil
    }
  }

  def apply(name: String): Unit@skipto = ???
}
