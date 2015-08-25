package inverse_macros

import scala.reflect.macros.whitebox
import language.existentials

private[inverse_macros] class Bundle(val c: whitebox.Context) {

  import c.universe._

  def annotationImpl(annottees: Expr[Any]*): Expr[Any] = {
    annottees match {
      case List(annottee) => annottee.tree match {
        case ddef: DefDef =>
          // 無限ループ防止の為、Flag.SYNTHETIC を付加する (これが付いているものは書き換え済みとみなして annotation を付加しない)

          val newMods = Modifiers(ddef.mods.flags | Flag.SYNTHETIC, ddef.mods.privateWithin, ddef.mods.annotations)
          val newRhs =
            ddef.rhs match {
              case Assign(_, _) =>
                // macro system のバグ (SI-8846) 回避 (名前付き/デフォルト引数のあるメソッドの誤検出)
                q"${ddef.rhs}; ()"
              case _ =>
                ddef.rhs
            }

          def transform(t: Tree) = q"inverse_macros.transform[${ddef.tpt}]($t)"

          def typecheck(t: Tree) =
            if (ddef.tpt.isEmpty)
              t
            else
              q"inverse_macros.typecheck[${ddef.tpt}]($t)"

          c.Expr[Any](q"$newMods def ${ddef.name}[..${ddef.tparams}](...${ddef.vparamss}): ${ddef.tpt} = ${typecheck(transform(newRhs))}")
        case _ =>
          c.abort(c.enclosingPosition, "not suitable annottee:\t" + annottee)
      }
      case _ => c.abort(c.enclosingPosition, "not suitable annottees:\t" + annottees)
    }
  }

  // 何か変換する
  def transformImpl(a: Tree): Tree =
    new inverse_macros.Transformer[c.type](c).imTransform {
      // Top level 要素の型がちゃんとついていない時があって困るのでリセットして付け直す
      // 多分 whitebox Context での大雑把な型付けが反映されているのだと思う (とりあえず Any であとで詳細に)
      if (a.tpe =:= definitions.AnyTpe) {
        c.internal.setType(a, null)
        c.typecheck(a)
      } else
        a
    }

  private[inverse_macros] final val util = new Util[c.type](c)

  // method の返り値型の annotation の validation をする
  // これをチェックしないとマクロ変換が行われなかったことの検出を runtime error で見るしかなくなる
  // やることは3つ
  // 1. 右辺値の tree の型についている annotation が省略されていないか (adaption で隠されないか)
  // 2. 右辺値の tree の型についている annotation と互換性があるか
  // 3. 宣言に複数個付いているものを検出する (右辺値の tree の方は変換エンジンのほうで検出する)
  def typecheckImpl[T: c.WeakTypeTag](a: Tree) =
    util.typeCompatibilityCheck(weakTypeOf[T])(a.tpe) match {
      case Some(message) =>
        c.abort(c.enclosingPosition, "Method type declaration compatibility check failed.\n\t" + message)
      case None =>
        a
    }

}
