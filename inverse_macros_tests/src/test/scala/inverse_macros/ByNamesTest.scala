package inverse_macros

import debug._
import org.scalatest.FunSuite

class ByNamesTest extends FunSuite {
  def intFunc(a: => Int) = ???

  def intAnnotFunc(a: => Int@test1) = ???

  def int = ??? : Int

  def boolean = ??? : Boolean

  def intAnnot = ??? : Int@test1

  def intAnnot2 = ??? : Int@test2

  def booleanAnnot = ??? : Boolean@test1

  def &&(b: Boolean) = ???

  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  test("by name -") {
    assert(expectException(parse("transform(intFunc(intAnnot))")))
    assert(expectException(parse("transform(boolean && booleanAnnot)")))
  }

  test("by name +") {
    compare(show(transform(intAnnotFunc(intAnnot))))("ByNamesTest.this.intAnnotFunc(ByNamesTest.this.intAnnot)")
    compare(show(transform(intAnnotFunc(int))))("ByNamesTest.this.intAnnotFunc(ByNamesTest.this.int)")
    compare(show(transform(booleanAnnot && boolean)))(
      "{ <synthetic> <artifact> val fresh$macro$1: Boolean = ByNamesTest.this.booleanAnnot; fresh$macro$1.&&(ByNamesTest.this.boolean) }"
    )
    compare(show(transform(&&(booleanAnnot))))(
      "{ <synthetic> <artifact> val fresh$macro$2: Boolean = ByNamesTest.this.booleanAnnot; ByNamesTest.this.&&(fresh$macro$2) }"
    )
  }


  // TODO: while / if / match / try validation

  test("if/match/try -") {
    assert(expectException(parse("transform(if (boolean) intAnnot else intAnnot2)")))
    assert(expectException(parse("transform(intFunc(if (boolean) intAnnot else intAnnot))")))
    assert(expectException(parse("transform(int match { case _ => intAnnot; case _ => intAnnot2})")))
    assert(expectException(parse("transform(intFunc(int match { case _ => intAnnot}))")))
    assert(expectException(parse("transform(try intAnnot catch {case _: Throwable => intAnnot2})")))
    assert(expectException(parse("transform(intFunc(try intAnnot catch {case _: Throwable => intAnnot}))")))
  }

  test("if/match/try +") {
    compare(show(transform(if (booleanAnnot) int else int)))(
      "{ <synthetic> <artifact> val $$: Boolean = ByNamesTest.this.booleanAnnot; if ($$) ByNamesTest.this.int else ByNamesTest.this.int }"
    )

    compare(show(transform(if (boolean) intAnnot else int)))(
      "(if (ByNamesTest.this.boolean) ByNamesTest.this.intAnnot else ByNamesTest.this.int: Int @inverse_macros.test1)"
    ) // this is a bug of show method: "(if (ByNamesTest.this.boolean) ByNamesTest.this.intAnnot else ByNamesTest.this.int): Int @inverse_macros.test1" is correct.

    compare(show(transform(if (boolean) int else intAnnot)))(
      "(if (ByNamesTest.this.boolean) ByNamesTest.this.int else ByNamesTest.this.intAnnot: Int @inverse_macros.test1)"
    ) // this is a bug of show method: "(if (ByNamesTest.this.boolean) ByNamesTest.this.int else ByNamesTest.this.intAnnot): Int @inverse_macros.test1" is correct.

    compare(show(transform(intAnnot match { case _ => int})))(
      "{ <synthetic> <artifact> val $$: Int = ByNamesTest.this.intAnnot; $$ match { case _ => ByNamesTest.this.int } }"
    )

    compare(show(transform(int match { case _ => intAnnot})))(
      "(ByNamesTest.this.int match { case _ => ByNamesTest.this.intAnnot }: Int @inverse_macros.test1)"
    ) // this is a bug of show method: "(ByNamesTest.this.int match { case _ => ByNamesTest.this.intAnnot }): Int @inverse_macros.test1" is correct.

    compare(show(transform(try intAnnot catch {
      case _: Throwable => int
    })))(
        "(try { ByNamesTest.this.intAnnot } catch { case (_: Throwable) => ByNamesTest.this.int }: Int @inverse_macros.test1)"
      ) // this is a bug of show method: "(try { ByNamesTest.this.intAnnot } catch { case (_: Throwable) => ByNamesTest.this.int }): Int @inverse_macros.test1" is correct.

    compare(show(transform(try int catch {
      case _: Throwable => intAnnot
    })))(
        "(try { ByNamesTest.this.int } catch { case (_: Throwable) => ByNamesTest.this.intAnnot }: Int @inverse_macros.test1)"
      ) // this is a bug of show method: "(try { ByNamesTest.this.int } catch { case (_: Throwable) => ByNamesTest.this.intAnnot }): Int @inverse_macros.test1" is correct.

    compare(show(transform(try intAnnot)))(
      "(try { ByNamesTest.this.intAnnot }: Int @inverse_macros.test1)"
    ) // this is a bug of show method: "(try { ByNamesTest.this.intAnnot }): Int @inverse_macros.test1" is correct.

    compare(show(transform(try intAnnot catch {
      case _: Throwable => intAnnot
    } finally intAnnot2)))(
        "(try { ByNamesTest.this.intAnnot } catch { case (_: Throwable) => ByNamesTest.this.intAnnot } finally { ByNamesTest.this.intAnnot2; () }: Int @inverse_macros.test1)"
      ) // this is a bug of show method: "(try { ByNamesTest.this.intAnnot } catch { case (_: Throwable) => ByNamesTest.this.intAnnot } finally { ByNamesTest.this.intAnnot2; () }): Int @inverse_macros.test1" is correct.

    compare(show(transform(try intAnnot finally intAnnot2)))(
      "(try { ByNamesTest.this.intAnnot } finally { ByNamesTest.this.intAnnot2; () }: Int @inverse_macros.test1)"
    ) // this is a bug of show method: "(try { ByNamesTest.this.intAnnot } finally { ByNamesTest.this.intAnnot2; () }): Int @inverse_macros.test1" is correct.

    compare(show(transform(if (boolean) intAnnot else booleanAnnot)))(
      "(if (ByNamesTest.this.boolean) ByNamesTest.this.intAnnot else ByNamesTest.this.booleanAnnot: AnyVal @inverse_macros.test1)"
    ) // this is a bug of show method: ...
  }

  test("complex -") {
    assert(expectException(parse("transform(int + intFunc(intAnnot))")))
    assert(expectException(parse("transform(booleanAnnot && (booleanAnnot && booleanAnnot))")))
  }

  test("complex +") {
    compare(show(transform(intFunc(int + intAnnot))))(
      "ByNamesTest.this.intFunc({ <synthetic> <artifact> val $$: Int = ByNamesTest.this.intAnnot; ByNamesTest.this.int.+($$) })"
    )
    compare(show(transform(booleanAnnot && (booleanAnnot && boolean))))(
      "{ <synthetic> <artifact> val $$: Boolean = ByNamesTest.this.booleanAnnot; $$.&&({ <synthetic> <artifact> val $$: Boolean = ByNamesTest.this.booleanAnnot; $$.&&(ByNamesTest.this.boolean) }) }"
    )
  }


  test("function -") {
    assert(expectException(parse("transform((a: Int@inverse_macros.test1) => a)")))
    assert(expectException(parse("transform((a: Int) => intFunc(intAnnot))")))
  }

  test("function +") {
    compare(show(transform{
      val func = (a: Int) => intAnnot
      println(func(int))
    }))(
      """{
        |  val func: Int => Int @inverse_macros.test1 = ((a: Int) => ByNamesTest.this.intAnnot);
        |  <synthetic> <artifact> val $$: Int = func.apply(ByNamesTest.this.int);
        |  scala.Predef.println($$)
        |}""".stripMargin
    )

    compare(show(transform{
      val func = (a: Int) => {
        println(intAnnot)
      }
      func(int)
    }))(
      """{
        |  val func: Int => Unit = ((a: Int) => {
        |     <synthetic> <artifact> val $$: Int = ByNamesTest.this.intAnnot;
        |     scala.Predef.println($$)
        |  });
        |  func.apply(ByNamesTest.this.int)
        |}""".stripMargin
      )

    compare(show(transform{
      println((a: Int) => intAnnot)
    }))(
        """{
          |  <synthetic> <artifact> val $$: Int => Int @inverse_macros.test1 =
          |    ((a: Int) => ByNamesTest.this.intAnnot);
          |  scala.Predef.println($$)
          |}""".stripMargin
      )
  }
}
