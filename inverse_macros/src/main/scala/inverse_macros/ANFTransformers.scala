package inverse_macros

trait ANFTransformers[C <: scala.reflect.macros.blackbox.Context] {
  this: Transformer[C] =>

  import c.universe._
  import c.internal._
  import util._

  private[this] type LTr = PartialFunction[(Tree, TypingTransformApi), List[Tree]]

  private[inverse_macros] def anfTransform(tree: Tree, api: TypingTransformApi) = anfTransformers((tree, api))

  // COMFRK vol.4 を参考にしたデザイン
  private[this] lazy val anfTransformers: LTr = Seq[LTr](
    fineGrainedApply, valDef, simpleConstructs, blockConstrucs, miscConstructs, default
  ) reduceLeft (_ orElse _)


  private[this] def defineVal(tree: Tree, api: TypingTransformApi): ValDef = {
    val name = TermName(c.freshName())
    import Flag._
    // c.typecheck に型付けと同時に symbol を割り当てる
    // api.typecheck では symbol が割り当てられない
    val tpe = removeIMAnnotations(tree.tpe)
    val newValdef = c.typecheck(q"${ARTIFACT | SYNTHETIC} val $name: $tpe = $tree").asInstanceOf[ValDef]
    // change owners of local variables
    c.internal.changeOwner(newValdef, newValdef.symbol.owner, api.currentOwner)
    c.internal.changeOwner(newValdef.rhs, api.currentOwner, newValdef.symbol)
    newValdef
  }

  private[this] def makeRef(valDef: ValDef) =
    gen.mkAttributedRef(valDef.symbol)

  private[this] def annotTrans[T <: Tree](tree: T)(fun: T => T) =
    if (detectAnnotatedTyped(tree))
      fun(tree)
    else
      tree

  private[this] def alwaysExpand[T <: Tree](tree: T, api: TypingTransformApi) =
    if (detectAnnotatedTyped(tree)) {
      val newDef = defineVal(tree, api)
      val newRef = makeRef(newDef)
      newDef :: newRef :: Nil
    } else
      tree :: Nil

  private[this] lazy val fineGrainedApply: LTr = {

    case (tree@FineGrainedApply(receiver, method, targs, argss), api) =>
      if (detectAnnotatedTyped(receiver) || argss.exists(_.exists(p => detectAnnotatedTyped(p._1)))) {
        val receiverList = alwaysExpand(receiver, api)
        //        var newTargs = targs
        val argListList =
          for (args <- argss) yield
            for ((arg, paramSymbol) <- args) yield
              if (paramSymbol.isByName) {
                val newArg = api.recur(arg)
                val paramType = paramSymbol.info match {
                  case tpe if !paramSymbol.asTerm.isByNameParam =>
                    tpe
                  case TypeRef(_, _, List(tpe)) =>
                    tpe
                }

                //                val pan = getIMAnnotation(paramType)
                //                val nan = getIMAnnotation(newArg.tpe)
                //                for ((pt, nt) <- pan.typeArgs.zip(nan.typeArgs) if pt.typeSymbol.isParameter)
                //                  newTargs = for ((t, a) <- newTargs.zip(tree.symbol.asMethod.typeParams)) yield
                //                    if (a == pt.typeSymbol && t.tpe =:= definitions.NothingTpe) {
                ////                      c.warning(c.enclosingPosition, t + " " + nt)
                //                      TypeTree(nt)
                //                    } else
                //                      t

                //                c.warning(c.enclosingPosition, targs + " " + newTargs)
                typeCompatibilityCheck(paramType)(newArg.tpe) match {
                  case Some(message) =>
                    c.abort(c.enclosingPosition, "Incompatible byname argument type:\n\t" + message)
                  case None =>
                    List(newArg)
                }
              } else
                alwaysExpand(arg, api)
        receiverList.init ++ argListList.map(_.map(_.init)).flatten.flatten :+
          api.typecheck(FineGrainedApply.copy(tree, receiverList.last, method, targs, argListList.map(_.map(_.last))))
      } else
        tree :: Nil

    case (tree@FineGrainedTypeApply(receiver, method, targs), api) =>
      expand(tree, receiver, api)(FineGrainedTypeApply.copy(_, _, method, targs))
  }

  private[this] lazy val valDef: LTr = {
    case (tree@ValDef(mods, name, tpt, rhs), api) =>
      if (mods.hasFlag(Flag.LAZY) && getIMAnnotations(tpt.tpe).nonEmpty)
        c.abort(c.enclosingPosition, "Sorry! Impure typed lazy is not supported.\n\t" + tpt)
      else if (detectAnnotatedTyped(rhs)) {
        val init :+ last =
          api.atOwner(tree.symbol) {
            anfTransform(rhs, api)
          }
        // 変数の型は inverse_macros annotation を持たない
        if (getIMAnnotations(tree.symbol.info).nonEmpty)
          setInfo(tree.symbol, removeIMAnnotations(tree.symbol.info))
        init :+ treeCopy.ValDef(tree, mods, name, TypeTree(tree.symbol.info), last)
      } else
        tree :: Nil
  }

  private[this] def expand(whole: Tree, sub: Tree, api: TypingTransformApi)(copier: (Tree, RefTree) => Tree) =
    if (detectAnnotatedTyped(sub)) {
      val newDef = defineVal(sub, api)
      val newRef = makeRef(newDef)
      newDef :: copier(whole, newRef) :: Nil
    } else
      whole :: Nil

  private[this] lazy val simpleConstructs: LTr = {
    case (tree@Select(qual, name), api) =>
      expand(tree, qual, api)(treeCopy.Select(_, _, name))

    case (tree@Return(expr), api) =>
      expand(tree, expr, api)(treeCopy.Return)

    case (tree@Throw(expr), api) =>
      expand(tree, expr, api)(treeCopy.Throw)

    case (tree@Assign(lhs, rhs), api) =>
      expand(tree, rhs, api)(treeCopy.Assign(_, lhs, _))
  }

  // :TODO knf of pat
  private[this] def transformCaseDef(caseDef: CaseDef, api: TypingTransformApi): CaseDef =
    treeCopy.CaseDef(caseDef, caseDef.pat, caseDef.guard, api.recur(caseDef.body))

  private[this] lazy val blockConstrucs: LTr = {
    case (tree@Block(_, _), api) =>
      api.recur(tree) :: Nil

    case (tree@Function(vparams, body), api) =>
      if (vparams.exists(param => getIMAnnotations(param.tpt.tpe).nonEmpty))
        c.abort(c.enclosingPosition, "Anonymous function parameter type is not inverse_macros annotatable:\n\t" + vparams)
      else
        treeCopy.Function(tree, vparams, api.recur(body)) :: Nil

    case (tree@CaseDef(_, _, _), api) =>
      transformCaseDef(tree, api) :: Nil

    // 2 段階で
    // 普通の引数 => byname 的な引数
    case (tree@If(cond, thenp, elsep), api) =>
      if (detectAnnotatedTyped(cond)) {
        val newDef = defineVal(cond, api)
        val newRef = makeRef(newDef)
        newDef :: treeCopy.If(tree, newRef, thenp, elsep) :: Nil
      } else {
        val newThenp = annotTrans(thenp)(api.recur)
        val newElsep = annotTrans(elsep)(api.recur)
        val newTree = typer(treeCopy.If(tree, cond, newThenp, newElsep), api)
        // compatibility check
        typesCompatibilityCheck(newTree.tpe)(newThenp.tpe, newElsep.tpe) match {
          case Some(message) =>
            c.abort(c.enclosingPosition, "Incompatible if branch types:\n\t" + message)
          case None =>
            newTree :: Nil
        }
      }

    case (tree@Match(selector, cases), api) =>
      if (detectAnnotatedTyped(selector)) {
        val newDef = defineVal(selector, api)
        val newRef = makeRef(newDef)
        newDef :: treeCopy.Match(tree, newRef, cases) :: Nil
      } else {
        val newCases = cases.map(annotTrans(_)(transformCaseDef(_, api)))
        val newTree = typer(treeCopy.Match(tree, selector, newCases), api)
        // compatibility check
        typesCompatibilityCheck(newTree.tpe)(newCases.map(_.tpe): _*) match {
          case Some(message) =>
            c.abort(c.enclosingPosition, "Incompatible match branch types:\n\t" + message)
          case None =>
            newTree :: Nil
        }
      }

    case (tree@Try(block, catches, finalizer), api) =>
      val newBlock = annotTrans(block)(api.recur)
      val newCatches = catches.map(annotTrans(_)(transformCaseDef(_, api)))
      val newFinalizer = annotTrans(finalizer)(api.recur)
      if (getIMAnnotations(newFinalizer.tpe).isEmpty) {
        val newTree = typer(treeCopy.Try(tree, newBlock, newCatches, newFinalizer), api)
        // compatibility check
        typesCompatibilityCheck(newTree.tpe)(newBlock.tpe :: newCatches.map(_.tpe): _*) match {
          case Some(message) =>
            c.abort(c.enclosingPosition, "Incompatible match branch types:\n\t" + message)
          case None =>
            newTree :: Nil
        }
      } else
      // TODO:
        c.abort(c.enclosingPosition, "Sorry! Finalyzer in Try support is incomplete.\n\t" + newFinalizer)
  }

  private[this] lazy val miscConstructs: LTr = {

    // eliminated by typechecker (-> typed (annotated type))
    /*
    // type annotation do not match
    case (tree@Annotated(annot, arg), api) =>
      if (detectAnnotatedTyped(arg)) {
        val init :+ last = anfTransform(arg, api)
        init :+ treeCopy.Annotated(tree, annot, last)
      } else
        tree :: Nil
        */

    case (tree@Typed(expr, tpt), api) =>
      if (detectAnnotatedTyped(expr)) {
        val init :+ last = anfTransform(expr, api)
        if (last.tpe =:= tpt.tpe)
          init :+ last
        else
          init :+ treeCopy.Typed(tree, last, tpt)
      } else
        tree :: Nil


    // COMFRK vol.4 を参考に while を復元
    // それ以外に @tailrec の場合があるみたいだけどこの phase では出てこない
    // 他の macro によって生成される場合はあるかもしれない
    /*
      def makeWhile(startPos: Int, cond: Tree, body: Tree): Tree = {
      val lname = freshTermName(nme.WHILE_PREFIX) // if termName.encodedName.toString.startsWith("while$") =>
      def default = wrappingPos(List(cond, body)) match {
        case p if p.isDefined => p.end
        case _                => startPos
      }
      val continu = atPos(o2p(body.pos pointOrElse default)) { Apply(Ident(lname), Nil) }
      val rhs = If(cond, Block(List(body), continu), Literal(Constant(())))
      LabelDef(lname, Nil, rhs)
    }
     */
    case (tree@LabelDef(name, params, rhs), api) =>
      if (detectAnnotatedTyped(rhs)) {
        val newRhs = api.recur(rhs)
        // TODO: STUB

        if (getIMAnnotations(newRhs.tpe).isEmpty)
          treeCopy.LabelDef(tree, name, params, newRhs) :: Nil
        else
          c.abort(c.enclosingPosition, "Sorry! While (LabelDef) support is incomplete.\n\t" + show(rhs))
      } else
        tree :: Nil

    // from cps plugin (https://github.com/scala/scala-continuations/blob/master/plugin/src/main/scala/scala/tools/selectivecps/SelectiveANFTransform.scala)
    /*

      // this is utterly broken: LabelDefs need to be considered together when transforming them to DefDefs:
      // suppose a Block {L1; ... ; LN}
      // this should become {D1def ; ... ; DNdef ; D1()}
      // where D$idef = def L$i(..) = {L$i.body; L${i+1}(..)}

      case ldef @ LabelDef(name, params, rhs) =>
        // println("trans LABELDEF "+(name, params, tree.tpe, hasAnswerTypeAnn(tree.tpe)))
        // TODO why does the labeldef's type have a cpsMinus annotation, whereas the rhs does not? (BYVALmode missing/too much somewhere?)
        if (hasAnswerTypeAnn(tree.tpe)) {
          // currentOwner.newMethod(name, tree.pos, Flags.SYNTHETIC) setInfo ldef.symbol.info
          val sym    = ldef.symbol resetFlag Flags.LABEL
          val rhs1   = rhs //new TreeSymSubstituter(List(ldef.symbol), List(sym)).transform(rhs)
          val rhsVal = transExpr(rhs1, None, getAnswerTypeAnn(tree.tpe))(getAnswerTypeAnn(tree.tpe).isDefined || isAnyParentImpure) changeOwner (currentOwner -> sym)

          val stm1 = localTyper.typed(DefDef(sym, rhsVal))
          // since virtpatmat does not rely on fall-through, don't call the labels it emits
          // transBlock will take care of calling the first label
          // calling each labeldef is wrong, since some labels may be jumped over
          // we can get away with this for now since the only other labels we emit are for tailcalls/while loops,
          // which do not have consecutive labeldefs (and thus fall-through is irrelevant)
          if (treeInfo.hasSynthCaseSymbol(ldef)) (List(stm1), localTyper.typed{Literal(Constant(()))}, cpsA)
          else {
            assert(params.isEmpty, "problem in ANF transforming label with non-empty params "+ ldef)
            (List(stm1), localTyper.typed{Apply(Ident(sym), List())}, cpsA)
          }
        } else {
          val rhsVal = transExpr(rhs, None, None)
          (Nil, updateSynthFlag(treeCopy.LabelDef(tree, name, params, rhsVal)), cpsA)
        }
     */

    // Unapply などパターンマッチは分解しない
  }

  private[this] lazy val default: LTr = {
    case (t, _) => List(t)
  }

  private[this] def typer(tree: Tree, api: TypingTransformApi): Tree =
    if (getIMAnnotations(tree.tpe).nonEmpty)
      tree
    else
      tree match {
        case If(cond, thenp, elsep) =>
          val thenpAnnot = getIMAnnotation(thenp.tpe)
          val elsepAnnot = getIMAnnotation(elsep.tpe)
          val newAnnot = api.typecheck(q"if($cond) (??? : $thenpAnnot) else (??? : $elsepAnnot)").tpe
          if (newAnnot == definitions.NothingTpe)
            tree
          else {
            //              setType(tree, c.typecheck(tq"${tree.tpe}@$newAnnot", c.TYPEmode).tpe)
            //              c.typecheck(tree)
            api.typecheck(q"$tree : @$newAnnot")
          }

        case Match(selector, cases) =>
          val newAnnots = cases.map(cs => getIMAnnotation(cs.tpe))
          val newAnnot = api.typecheck(q"$selector match {case ..${newAnnots.map(tpe => cq"_ => ??? : $tpe")}}").tpe
          if (newAnnot == definitions.NothingTpe)
            tree
          else {
            //              setType(tree, c.typecheck(tq"${tree.tpe}@$newAnnot", c.TYPEmode).tpe)
            //              c.typecheck(tree)
            api.typecheck(q"$tree : @$newAnnot")
          }

        case Try(block, catches, finalizer) =>
          if (getIMAnnotations(finalizer.tpe).isEmpty) {
            val blockAnnot = getIMAnnotation(block.tpe)
            val caseAnnots = catches.map(cs => getIMAnnotation(cs.tpe))
            val newAnnot = api.typecheck(q"try {??? : $blockAnnot} catch {case ..${caseAnnots.map(tpe => cq"_ => ??? : $tpe")}}").tpe
            if (newAnnot == definitions.NothingTpe)
              tree
            else {
              //              setType(tree, c.typecheck(tq"${tree.tpe}@$newAnnot", c.TYPEmode).tpe)
              //              c.typecheck(tree)
              api.typecheck(q"$tree : @$newAnnot")
            }
          } else
          // TODO:
            c.abort(c.enclosingPosition, "Sorry! Finalyzer in Try support is incomplete.\n\t" + finalizer)
      }
}
