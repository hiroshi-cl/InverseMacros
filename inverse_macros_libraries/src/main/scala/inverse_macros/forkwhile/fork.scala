package inverse_macros.forkwhile

import inverse_macros.{IMAnnotation, IMTransformer}

import scala.concurrent.Future
import scala.language.higherKinds

class fork extends IMAnnotation

object fork extends IMTransformer {

  import scala.reflect.macros.blackbox


  override def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                        (api: c.internal.TypingTransformApi)
                        (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) = {
    import c.universe._

    val ccsym = symbolOf[ForkContext]
    val cpsym = symbolOf[fork]

    head match {
      case ValDef(mods, name, tpt, rhs) =>
        val transformed = api.recur(api.typecheck(q"{..$cont}"))
        val ex = TermName(c.freshName("fork_ex_vd"))
        val tryCatch = api.typecheck(q"(try {$transformed; throw new $ccsym(Nil)} catch { case $ex: $ccsym => throw new $ccsym(${head.symbol} :: $ex.list) }): Unit@join")
        List(head, tryCatch) -> Nil

      case _ =>
        val x = TermName(c.freshName("x"))
        val newHead = c.typecheck(q"val $x: Future[Unit] = $head")
        c.internal.setOwner(newHead.symbol, api.currentOwner)
        c.internal.changeOwner(head, api.currentOwner, newHead.symbol)
        val transformed = api.recur(api.typecheck(q"{..$cont}"))
        val ex = TermName(c.freshName("fork_ex_"))
        val tryCatch = api.typecheck(q"(try {$transformed; throw new $ccsym(Nil)} catch { case $ex: $ccsym => throw new $ccsym(${newHead.symbol} :: $ex.list) }): Unit@join")
        List(newHead, tryCatch) -> Nil
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def apply(body: => Unit): Future[Unit]@fork = Future(body)
}
