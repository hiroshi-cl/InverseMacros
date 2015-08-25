package inverse_macros

private[inverse_macros] final class Util[C <: scala.reflect.macros.blackbox.Context](val c: C) {

  import c.universe._
  import c.internal._

  def removeIMAnnotations(tpe: Type) =
  try {
    tpe match {
      case AnnotatedType(annotations, underlying) =>
        // annotation の引数情報消えちゃうけどいいのかな？
        val newAnnotations = annotations.filterNot(_.tree.tpe <:< typeOf[IMAnnotation]).map(_.tree.tpe)
        if (newAnnotations.isEmpty)
          underlying
        else
          c.typecheck(tq"$underlying@..$newAnnotations", mode = c.TYPEmode).tpe
      case _ =>
        tpe
    }
  } catch {
    case e: Throwable => throw new Exception("What happened?\t" + tpe, e)
  }


  def getIMAnnotations(tpe: Type) =
    tpe match {
      case AnnotatedType(annotations, _) =>
        annotations.filter(_.tree.tpe <:< typeOf[IMAnnotation]).map(_.tree.tpe).filterNot(_.toString.equals("<error>"))
      case _ =>
        Nil
    }

  def getIMAnnotation(tpe: Type) =
    getIMAnnotations(tpe) match {
      case Nil =>
        definitions.NothingTpe
      case annot :: Nil =>
        annot
      case annots =>
        c.abort(c.enclosingPosition, "Multiple inverse_macros annotation is not allowed: " + annots)
    }

  def getIMAnnotationTree(tpe: Type) =
    tpe match {
      case AnnotatedType(annotations, _) =>
        annotations.filter(_.tree.tpe <:< typeOf[IMAnnotation]) match {
          case annotation :: Nil =>
            annotation.tree
          case _ :: _ =>
            c.abort(c.enclosingPosition, "Multiple inverse_macros annotation is not allowed: " + tpe)
          case _ =>
            c.abort(c.enclosingPosition, "No inverse_macros annotation is detected: " + tpe)
        }
      case _ =>
        c.abort(c.enclosingPosition, "Not annoted: " + tpe)
    }

  def getUserMacro(tpe: Type): (IMTransformer, List[Type], List[List[Tree]]) = {
    // 一旦文字列に変換するあたり (特に show のバグ) とかちょっと気になるけど SI-9218 避け
    val q"new ${tpt: Tree}(...${argss: List[List[Tree]]})" = getIMAnnotationTree(tpe)
    val tp = tpt.tpe.typeConstructor
    //    c.abort(c.enclosingPosition, show(TypeTree(tp)))
    (c.eval(c.Expr[IMTransformer](c.parse(show(TypeTree(tp))))), tpt.tpe.typeArgs, argss)
  }

  def typeCompatibilityCheck(outer: Type)(inner: Type): Option[String] =
    getIMAnnotations(outer) match {
      case Nil =>
        val inner_annotations = getIMAnnotations(inner)
        if (inner_annotations.nonEmpty)
          Some("inverse_macros annotation " + inner_annotations + " is not omittable: " + outer)
        else
          None

      case outer_tpe :: Nil =>
        getIMAnnotations(inner) match {
          case Nil =>
            None

          case inner_tpe :: Nil =>
//            if (!(inner_tpe.typeConstructor =:= outer_tpe.typeConstructor))
//              Some("Incompatible inverse_macros annotation type constructor: " +
//                inner_tpe.typeConstructor + " !=:= " + outer_tpe.typeConstructor)
//            else if (!(inner_tpe <:< outer_tpe))
//            //              Some("Incompatible inverse_macros annotation type argument: " + inner_tpe + " !<:< " + outer_tpe)
//              None // relaxed for shift/reset typing
//            else
              None

          case inner_annotations =>
            Some("Invalid inner type. Multiple inverse_macros annotation is not allowed: " + inner_annotations)
        }

      case outer_annotations =>
        Some("Invalid outer type. Multiple inverse_macros annotation is not allowed: " + outer_annotations)
    }


  def typesCompatibilityCheck(outer: Type)(inners: Type*): Option[String] =
    inners.map(typeCompatibilityCheck(outer)(_)).collectFirst({ case Some(message) => message})

  // by-name (!attention! "&&" and "||" do not contains by-name flag!)
  final case class IsByName(value: Boolean)

  implicit class GetIsByName(symbol: Symbol) {
    def isByName =
      if (symbol == NoSymbol)
        false
      else
        attachments(symbol).get[IsByName].getOrElse {
          c.warning(symbol.pos, "by-name flag undefined: " + symbol)
          IsByName(false)
        }.value
  }

  // copy and modify from TreeInfo in the scala compiler
  final class FineGrainedApply(val tree: Apply) {

    val callee: Tree = {
      def loop(tree: Tree): Tree = tree match {
        case Apply(fn, _) => loop(fn)
        case t => t
      }
      loop(tree)
    }

    val tapply: FineGrainedTypeApply = new FineGrainedTypeApply(callee)

    val argss: List[List[Tree]] = {
      def loop(tree: Tree): List[List[Tree]] = tree match {
        case Apply(fn, args) => loop(fn) :+ args
        case _ => Nil
      }
      loop(tree)
    }

    def isBooleanSC(symbol: Symbol): Boolean = {
      val and = definitions.BooleanTpe.member(TermName("&&").encodedName)
      val or = definitions.BooleanTpe.member(TermName("||").encodedName)
      if (symbol == and || symbol == or)
        true
      else
        false
    }

    // by-name (!attention! "&&" and "||" do not contains by-name flag!)
    for (paramList <- tapply.core.tpe.paramLists)
      for (paramSymbol <- paramList)
        if (isBooleanSC(callee.symbol))
          updateAttachment(paramSymbol, IsByName(true))
        else
          updateAttachment(paramSymbol, IsByName(paramSymbol.asTerm.isByNameParam))
  }

  object FineGrainedApply {
    def unapply(tree: Tree): Option[(Tree, TermName, List[Tree], List[List[(Tree, Symbol)]])] =
      tree match {
        case t@Apply(_, _) =>
          val applied = new FineGrainedApply(t)
          val tapply = applied.tapply
          Some((tapply.receiver, tapply.method, tapply.targs,
            applied.argss.zip(tapply.core.tpe.paramLists).map(p => p._1.zipAll(p._2, EmptyTree, NoSymbol))))
        case _ =>
          None
      }

    def copy(tree: Tree, receiver: Tree, method: TermName,
             targs: List[Tree], argss: List[List[Tree]]): Tree = {

      def applyChain(t: Tree, a: List[List[Tree]]): Tree = t match {
        case Apply(fn, _) =>
          treeCopy.Apply(t, applyChain(fn, a.init), a.last)
        case _ =>
          FineGrainedTypeApply.copy(t, receiver, method, targs)
      }

      applyChain(tree, argss).asInstanceOf[Apply]
    }
  }

  final class FineGrainedTypeApply(val tree: Tree) {
    val core: Tree = tree match {
      case TypeApply(fn, _) => fn
      case AppliedTypeTree(fn, _) => fn
      case tree => tree
    }

    val targs: List[Tree] = tree match {
      case TypeApply(_, args) => args
      case AppliedTypeTree(_, args) => args
      case _ => Nil
    }

    val receiver: Tree = core match {
      case Select(r, _) => r
      case Ident(_) => EmptyTree
      case _ =>
        c.abort(c.enclosingPosition, "Invalid apply tree: " + tree)
    }

    val method: TermName = core match {
      case Select(_, m) => m.toTermName
      case Ident(m) => m.toTermName
      case _ =>
        c.abort(c.enclosingPosition, "Invalid apply tree: " + tree)
    }
  }


  object FineGrainedTypeApply {
    def unapply(tree: Tree): Option[(Tree, TermName, List[Tree])] =
      tree match {
        case TypeApply(_, _) | AppliedTypeTree(_, _) =>
          val tapply = new FineGrainedTypeApply(tree)
          Some((tapply.receiver, tapply.method, tapply.targs))
        case _ =>
          None
      }

    def copy(tree: Tree, receiver: Tree, method: TermName, targs: List[Tree]): Tree = {

      def tapplyChain(t: Tree): Tree = t match {
        case TypeApply(fn, _) =>
          treeCopy.TypeApply(t, core(fn), targs)
        case AppliedTypeTree(fn, _) =>
          treeCopy.AppliedTypeTree(t, core(fn), targs)
        case fn => core(fn)
      }

      def core(t: Tree): Tree = t match {
        case Select(_, _) => treeCopy.Select(t, receiver, method)
        case Ident(_) => treeCopy.Ident(tree, method)
        case _ =>
          c.abort(c.enclosingPosition, "Invalid type apply tree: " + tree)
      }

      tapplyChain(tree)
    }
  }

}
