package inverse_macros

import debug._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class CPSIdentTest extends FunSuite {
  def intFunc1(a: Int): Unit = ???

  def intFunc2(a: Int)(b: Int): Unit = ???

  def intFunc3(a: Int)(b: Int, c: Int = 30): Unit = ???

  def int: Int = ???

  def boolean: Boolean = ???

  def intFunc1annot(a: Int): (Unit@cpsIdent) = ???

  def intFunc2annot(a: Int)(b: Int): (Unit@cpsIdent) = ???

  def intFunc3annot(a: Int)(b: Int, c: Int = 30): (Unit@cpsIdent) = ???

  def intAnnot: (Int@cpsIdent) = ???

  def booleanAnnot: (Boolean@cpsIdent) = ???

  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  test("simple @") {
    compare(show(transform(println(intFunc1annot(10)))))(
      """{
        |  @inline <synthetic> def $$($$: Unit): Unit = {
        |    <synthetic> <artifact> val $$: Unit = $$;
        |    scala.Predef.println($$)
        |  };
        |  $$(CPSIdentTest.this.intFunc1annot(10))
        |}""".stripMargin)

    compare(show(transform(println(intFunc2annot(10)(20)))))(
      """{
        |  @inline <synthetic> def $$($$: Unit): Unit = {
        |    <synthetic> <artifact> val $$: Unit = $$;
        |    scala.Predef.println($$)
        |  };
        |  $$(CPSIdentTest.this.intFunc2annot(10)(20))
        |}""".stripMargin)

    compare(show(transform(println(intFunc3annot(10)(20)))))(
      """{
        |  @inline <synthetic> def $$($$: Unit): Unit = {
        |    <synthetic> <artifact> val $$: Unit = $$;
        |    scala.Predef.println($$)
        |  };
        |  $$({
        |    <artifact> val x$1: Int = 10;
        |    <artifact> val x$2: Int = 20;
        |    <artifact> val x$3: Int = CPSIdentTest.this.intFunc3annot$default$3(x$1);
        |    CPSIdentTest.this.intFunc3annot(x$1)(x$2, x$3)
        |  })
        |}""".stripMargin)

    compare(show(transform(println(intFunc3annot(10)(b = 20)))))(
      """{
        |  @inline <synthetic> def $$($$: Unit): Unit = {
        |    <synthetic> <artifact> val $$: Unit = $$;
        |    scala.Predef.println($$)
        |  };
        |  $$({
        |    <artifact> val x$4: Int = 10;
        |    <artifact> val x$5: Int = 20;
        |    <artifact> val x$6: Int = CPSIdentTest.this.intFunc3annot$default$3(x$4);
        |    CPSIdentTest.this.intFunc3annot(x$4)(x$5, x$6)
        |  })
        |}""".stripMargin)

    compare(show(transform(println(intFunc3annot(10)(20, c = 30)))))(
      """{
        |  @inline <synthetic> def $$($$: Unit): Unit = {
        |    <synthetic> <artifact> val $$: Unit = $$;
        |    scala.Predef.println($$)
        |  };
        |  $$(CPSIdentTest.this.intFunc3annot(10)(20, 30))
        |}""".stripMargin)

    compare(show(transform(println(("hoge": @cpsIdent) + 1))))(
      """{
        |  @inline <synthetic> def $$($$: String("hoge")): Unit = {
        |    <synthetic> <artifact> val $$: String("hoge") = $$;
        |    <synthetic> <artifact> val $$: String = $$.+(1);
        |    scala.Predef.println($$)
        |  };
        |  $$(("hoge": String("hoge") @inverse_macros.cpsIdent))
        |}""".stripMargin)

    compare(show(transform(println(Math.sqrt(10): @cpsIdent))))(
      """{
        |  @inline <synthetic> def $$($$: Double): Unit = {
        |    <synthetic> <artifact> val $$: Double = $$;
        |    scala.Predef.println($$)
        |  };
        |  $$((java.lang.Math.sqrt(10.0): Double @inverse_macros.cpsIdent))
        |}""".stripMargin)

    compare(show(transform(println(Array(10)(0): @cpsIdent))))(
      """{
        |  @inline <synthetic> def $$($$: Int): Unit = {
        |    <synthetic> <artifact> val $$: Int = $$;
        |    scala.Predef.println($$)
        |  };
        |  $$((scala.Array.apply(10).apply(0): Int @inverse_macros.cpsIdent))
        |}""".stripMargin)

    compare(show(transform(this.intFunc1annot(10))))("this.intFunc1annot(10)")

    compare(show(transform {
      println(intFunc1annot(10))
      intFunc1annot(20)
    }))(
        """{
          |  @inline <synthetic> def $$($$: Unit): Unit @inverse_macros.cpsIdent = {
          |    <synthetic> <artifact> val $$: Unit = $$;
          |    scala.Predef.println($$);
          |    CPSIdentTest.this.intFunc1annot(20)
          |  };
          |  $$(CPSIdentTest.this.intFunc1annot(10))
          |}""".stripMargin)
  }

  test("simple *") {
    try {
      transform(println(intFunc1annot(10)))

      transform(println(intFunc2annot(10)(20)))

      transform(println(intFunc3annot(10)(20)))

      transform(println(intFunc3annot(10)(b = 20)))

      transform(println(intFunc3annot(10)(20, c = 30)))

      transform(println(("hoge": @cpsIdent) + 1))

      transform(println(Math.sqrt(10): @cpsIdent))

      transform(println(Array(10)(0): @cpsIdent))

      transform(this.intFunc1annot(10))

      transform {
        println(intFunc1annot(10))
        intFunc1annot(20)
      }
    } catch {
      case e: Throwable =>
    }
  }

  test("repeat @") {
    compare(show(transform {
      {
        intFunc2annot(10)(20)
      }
      val b = {
        intFunc2annot(10)(20)
        intFunc2(10)(20)
        intFunc2annot(10)(20)
        intFunc2(10)(20)
        intFunc2annot(10)(20)
      }
      intFunc2annot(10)(20)
    }))(
        """{
          |  @inline <synthetic> def $$(): Unit @inverse_macros.cpsIdent = {
          |    @inline <synthetic> def $$($$: Unit): Unit @inverse_macros.cpsIdent = {
          |      val b: Unit = $$;
          |      CPSIdentTest.this.intFunc2annot(10)(20)
          |    };
          |    $$({
          |      @inline <synthetic> def $$(): Unit @inverse_macros.cpsIdent = {
          |        CPSIdentTest.this.intFunc2(10)(20);
          |        @inline <synthetic> def $$(): Unit @inverse_macros.cpsIdent = {
          |          CPSIdentTest.this.intFunc2(10)(20);
          |          CPSIdentTest.this.intFunc2annot(10)(20)
          |        };
          |        CPSIdentTest.this.intFunc2annot(10)(20);
          |        $$()
          |      };
          |      CPSIdentTest.this.intFunc2annot(10)(20);
          |      $$()
          |    })
          |  };
          |  CPSIdentTest.this.intFunc2annot(10)(20);
          |  $$()
          |}""".stripMargin)

    compare(show(transform {
      val _1 = intFunc2annot(10)(20)
      val _2 = intFunc2(10)(20)
      val _3 = intFunc2annot(10)(20)
      val _4 = intFunc2(10)(20)
      val _5 = intFunc2annot(10)(20)
    }))(
        """{
          |  @inline <synthetic> def $$($$: Unit): Unit = {
          |    val _1: Unit = $$;
          |    val _2: Unit = CPSIdentTest.this.intFunc2(10)(20);
          |    @inline <synthetic> def $$($$: Unit): Unit = {
          |      val _3: Unit = $$;
          |      val _4: Unit = CPSIdentTest.this.intFunc2(10)(20);
          |      @inline <synthetic> def $$($$: Unit): Unit = {
          |        val _5: Unit = $$;
          |        ()
          |      };
          |      $$(CPSIdentTest.this.intFunc2annot(10)(20))
          |    };
          |    $$(CPSIdentTest.this.intFunc2annot(10)(20))
          |  };
          |  $$(CPSIdentTest.this.intFunc2annot(10)(20))
          |}""".stripMargin)
  }

  test("repeat *") {
    try {
      transform {
        {
          intFunc2annot(10)(20)
        }
        val b = {
          intFunc2annot(10)(20)
          intFunc2(10)(20)
          intFunc2annot(10)(20)
          intFunc2(10)(20)
          intFunc2annot(10)(20)
        }
        intFunc2annot(10)(20)
      }

      transform {
        val _1 = intFunc2annot(10)(20)
        val _2 = intFunc2(10)(20)
        val _3 = intFunc2annot(10)(20)
        val _4 = intFunc2(10)(20)
        val _5 = intFunc2annot(10)(20)
      }
    } catch {
      case e: Throwable =>
    }
  }

  println(show(transform {
    println(intFunc1annot(10))
    if (true)
      () : @cpsIdent
    else
      () : @cpsIdent
  }))

  println(show(transform {
    println(intFunc1annot(10))
    if (true)
      () : @cpsIdent
    else
      () : @cpsIdent
  } : Unit@cpsIdent))
}
