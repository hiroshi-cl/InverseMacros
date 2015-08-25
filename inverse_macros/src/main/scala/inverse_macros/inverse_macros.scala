// plugin とは別の場所で定義することで @inverse_macros に再帰的に変換が適用されることを防ぐ

import scala.language.experimental.macros
import scala.annotation.{TypeConstraint, StaticAnnotation}

package object inverse_macros {
  def transform[T](a: T): T = macro Bundle.transformImpl

  def typecheck[T](a: T): T = macro Bundle.typecheckImpl[T]
}

package inverse_macros {

class inverseMacroEngine extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Bundle.annotationImpl
}

trait IMAnnotation extends StaticAnnotation with TypeConstraint

trait IMTransformer {

  import scala.reflect.macros.blackbox

  def transform(c: blackbox.Context)(targs: List[c.Type], argss: List[List[c.Tree]])
                      (api: c.internal.TypingTransformApi)
                      (head: c.Tree, cont: List[c.Tree]): (List[c.Tree], List[c.Tree]) =
    List(head) -> cont
}

class IMSynth extends StaticAnnotation

}