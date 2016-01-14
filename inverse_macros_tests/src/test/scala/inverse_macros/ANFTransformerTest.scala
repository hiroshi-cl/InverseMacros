package inverse_macros

import debug._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class ANFTransformerTest extends FunSuite {
  def intFunc1(a: Int): Unit = ???

  def intFunc2(a: Int)(b: Int): Unit = ???

  def intFunc3(a: Int)(b: Int, c: Int = 30): Unit = ???

  def int: Int = ???

  def boolean: Boolean = ???

  def intFunc1annot(a: Int): (Unit@test1) = ???

  def intFunc2annot(a: Int)(b: Int): (Unit@test1) = ???

  def intFunc3annot(a: Int)(b: Int, c: Int = 30): (Unit@test1) = ???

  def intAnnot: (Int@test1) = ???

  def booleanAnnot: (Boolean@test1) = ???

  def compare(s1: String)(s2: String) = {
    val t1 = replaceFreshVariables(s1.replaceAll("\\s+", " "))
    val t2 = replaceFreshVariables(s2.replaceAll("\\s+", " "))
    assert(t1 == t2)
  }

  implicit def toBoolean(i: Int): Boolean = if (i == 0) false else true

  test("simple +") {
    compare(show(transform(println(intFunc1(10)))))("scala.Predef.println(ANFTransformerTest.this.intFunc1(10))")
    compare(show(transform(println(intFunc2(10)(20)))))("scala.Predef.println(ANFTransformerTest.this.intFunc2(10)(20))")
    compare(show(transform(println(intFunc3(10)(20)))))(
      """scala.Predef.println({
        |  <artifact> val x$1: Int = 10;
        |  <artifact> val x$2: Int = 20;
        |  <artifact> val x$3: Int = ANFTransformerTest.this.intFunc3$default$3(x$1);
        |  ANFTransformerTest.this.intFunc3(x$1)(x$2, x$3)
        |})""".stripMargin)
    compare(show(transform(println(intFunc3(10)(b = 20)))))(
      """scala.Predef.println({
        |  <artifact> val x$4: Int = 10;
        |  <artifact> val x$5: Int = 20;
        |  <artifact> val x$6: Int = ANFTransformerTest.this.intFunc3$default$3(x$4);
        |  ANFTransformerTest.this.intFunc3(x$4)(x$5, x$6)
        |})""".stripMargin)
    compare(show(transform(println(intFunc3(10)(20, c = 30)))))("scala.Predef.println(ANFTransformerTest.this.intFunc3(10)(20, 30))")
    compare(show(transform(println("hoge" + 1))))("scala.Predef.println(\"hoge\".+(1))")
    compare(show(transform(println(Math.sqrt(10)))))("scala.Predef.println(java.lang.Math.sqrt(10.0))")
    compare(show(transform(println(Array(10)(0)))))("scala.Predef.println(scala.Array.apply(10).apply(0))")
    compare(show(transform(this.intFunc1(10))))("this.intFunc1(10)")
  }

  test("simple @") {
    compare(show(transform(println(intFunc1annot(10)))))(
      """{
        |  <synthetic> <artifact> val $$: Unit =
        |    ANFTransformerTest.this.intFunc1annot(10);
        |  scala.Predef.println($$)
        |}""".stripMargin)

    compare(show(transform(println(intFunc2annot(10)(20)))))(
      """{
        | <synthetic> <artifact> val $$: Unit =
        |   ANFTransformerTest.this.intFunc2annot(10)(20);
        | scala.Predef.println($$)
        |}""".stripMargin)

    compare(show(transform(println(intFunc3annot(10)(20)))))(
      """{
        |  <synthetic> <artifact> val $$: Unit = {
        |    <artifact> val x$7: Int = 10;
        |    <artifact> val x$8: Int = 20;
        |    <artifact> val x$9: Int = ANFTransformerTest.this.intFunc3annot$default$3(x$7);
        |    ANFTransformerTest.this.intFunc3annot(x$7)(x$8, x$9)
        |  };
        |  scala.Predef.println($$)
        |}""".stripMargin)

    compare(show(transform(println(intFunc3annot(10)(b = 20)))))(
      """{
        |  <synthetic> <artifact> val $$: Unit = {
        |    <artifact> val x$10: Int = 10;
        |    <artifact> val x$11: Int = 20;
        |    <artifact> val x$12: Int = ANFTransformerTest.this.intFunc3annot$default$3(x$10);
        |    ANFTransformerTest.this.intFunc3annot(x$10)(x$11, x$12)
        |  };
        |  scala.Predef.println($$)
        |}""".stripMargin)

    compare(show(transform(println(intFunc3annot(10)(20, c = 30)))))(
      """{
        |  <synthetic> <artifact> val $$: Unit = ANFTransformerTest.this.intFunc3annot(10)(20, 30);
        |  scala.Predef.println($$)
        |}""".stripMargin)

    compare(show(transform(println(("hoge": @test1) + 1))))(
      """{
        |  <synthetic> <artifact> val $$: String("hoge") =
        |    ("hoge": String("hoge") @inverse_macros.test1);
        |  <synthetic> <artifact> val $$: String = $$.+(1);
        |  scala.Predef.println($$)
        |}""".stripMargin)

    compare(show(transform(println(Math.sqrt(10): @test1))))(
      """{
        |  <synthetic> <artifact> val $$: Double = (java.lang.Math.sqrt(10.0): Double @inverse_macros.test1);
        |  scala.Predef.println($$)
        |}""".stripMargin)

    compare(show(transform(println(Array(10)(0): @test1))))(
      """{
        |  <synthetic> <artifact> val $$: Int = (scala.Array.apply(10).apply(0): Int @inverse_macros.test1);
        |  scala.Predef.println($$)
        |}""".stripMargin)

    compare(show(transform(this.intFunc1annot(10))))("this.intFunc1annot(10)")

  }

  test("block +") {
    compare(show(transform {
      println(Math.sqrt(10))
      println("hoge" + 1)
    }))(
        """{
          |  scala.Predef.println(java.lang.Math.sqrt(10.0));
          |  scala.Predef.println("hoge".+(1))
          |}""".stripMargin)

    compare(show(transform {
      (boolean, int) match {
        case (a, b) => false;
        case _ => true
      }
    }))(
        """scala.Tuple2.apply[Boolean, Int](ANFTransformerTest.this.boolean, ANFTransformerTest.this.int) match {
          |  case (_1: Boolean, _2: Int)(Boolean, Int)((a @ _), (b @ _)) => false
          |  case _ => true
          |}""".stripMargin)

    compare(show(transform {
      val a: String = "piyo"
      println(a)
    }))(
        """{
          |  val a: String = "piyo";
          |  scala.Predef.println(a)
          |}""".stripMargin)

    compare(show(transform {
      val p = intFunc2(10)(20)
    }))(
        """{
          |  val p: Unit = ANFTransformerTest.this.intFunc2(10)(20);
          |  ()
          |}""".stripMargin)

    compare(show(transform {
      val a = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))( """{
           |  val a: Int = {
           |    val a: Int = 10;
           |    a.+(20)
           |  }.+(30);
           |  scala.Predef.println(a);
           |  a
           |}""".stripMargin)

    compare(show(transform {
      val a = 5
      val b = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))(
        """{
          |  val a: Int = 5;
          |  val b: Int = {
          |    val a: Int = 10;
          |    a.+(20)
          |  }.+(30);
          |  scala.Predef.println(a);
          |  a
          |}""".stripMargin)
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
        |  ANFTransformerTest.this.intFunc2annot(10)(20);
        |  val b: Unit = {
        |    ANFTransformerTest.this.intFunc2annot(10)(20);
        |    ANFTransformerTest.this.intFunc2(10)(20);
        |    ANFTransformerTest.this.intFunc2annot(10)(20);
        |    ANFTransformerTest.this.intFunc2(10)(20);
        |    ANFTransformerTest.this.intFunc2annot(10)(20)
        |  };
        |  ANFTransformerTest.this.intFunc2annot(10)(20)
        |}""".stripMargin)

    compare(show(transform {
      val _1 = intFunc2annot(10)(20)
      val _2 = intFunc2(10)(20)
      val _3 = intFunc2annot(10)(20)
      val _4 = intFunc2(10)(20)
      val _5 = intFunc2annot(10)(20)
    }))(
        """{
          |  val _1: Unit = ANFTransformerTest.this.intFunc2annot(10)(20);
          |  val _2: Unit = ANFTransformerTest.this.intFunc2(10)(20);
          |  val _3: Unit = ANFTransformerTest.this.intFunc2annot(10)(20);
          |  val _4: Unit = ANFTransformerTest.this.intFunc2(10)(20);
          |  val _5: Unit = ANFTransformerTest.this.intFunc2annot(10)(20);
          |  ()
          |}""".stripMargin)
  }

  test("type apply +") {
    compare(show(transform (None.asInstanceOf[Option[Int]])))("scala.None.asInstanceOf[Option[Int]]")
    compare(show(transform (None.isInstanceOf[Option[Int]])))("scala.None.isInstanceOf[Option[Int]]")
  }

  test("type apply @") {
    compare(show(transform (None.asInstanceOf[Option[Int]@test1])))(
      "scala.None.asInstanceOf[Option[Int] @inverse_macros.test1]")
    compare(show(transform (None.isInstanceOf[Option[Int]@test1])))(
      "scala.None.isInstanceOf[Option[Int] @inverse_macros.test1]")

    compare(show(transform (println(None.asInstanceOf[Option[Int]@test1]))))(
      """{
        |  <synthetic> <artifact> val $$: Option[Int] =
        |    scala.None.asInstanceOf[Option[Int] @inverse_macros.test1];
        |  scala.Predef.println($$)
        |}""".stripMargin)
  }

}
