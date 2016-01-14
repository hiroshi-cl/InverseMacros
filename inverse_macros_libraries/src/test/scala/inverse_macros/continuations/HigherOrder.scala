package inverse_macros.continuations

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.annotation._
import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, implicitConversions}
import scala.util.control.ControlThrowable

class HigherOrder extends JUnitSuite  {

  import java.util.concurrent.atomic._

  @Test def t5472 = {
    val map = Map("foo" -> 1, "bar" -> 2)
    reset[Unit,Unit] {
      val mapped =
        for {
          (location, accessors) <- new ContinuationizedParallelIterable(map)
        } yield {
          shiftUnit0[Int, Unit](23)
        }
      assertEquals(List(23, 23), mapped.toList)
    }
  }

  @deprecated("Suppress warnings", since = "2.11")
  final class ContinuationizedParallelIterable[+A](protected val underline: Iterable[A]) {
    def toList = underline.toList.sortBy(_.toString)

    final def filter(p: A => Boolean @suspendable): ContinuationizedParallelIterable[A] @suspendable =
      shift(
        new AtomicInteger(1) with ((ContinuationizedParallelIterable[A] => Unit) => Unit) {
          private val results = new AtomicReference[List[A]](Nil)

          @tailrec
          private def add(element: A) {
            val old = results.get
            if (!results.compareAndSet(old, element :: old)) {
              add(element)
            }
          }

          override final def apply(continue: ContinuationizedParallelIterable[A] => Unit) {
            for (element <- underline) {
              super.incrementAndGet()
              reset[Unit,Unit] {
                val pass = p(element)
                if (pass) {
                  add(element)
                }
                if (super.decrementAndGet() == 0) {
                  continue(new ContinuationizedParallelIterable(results.get))
                }
              }
            }
            if (super.decrementAndGet() == 0) {
              continue(new ContinuationizedParallelIterable(results.get))
            }
          }
        })

    final def foreach[U](f: A => U @suspendable): Unit @suspendable =
      shift(
        new AtomicInteger(1) with ((Unit => Unit) => Unit) {
          override final def apply(continue: Unit => Unit) {
            for (element <- underline) {
              super.incrementAndGet()
              reset[Unit,Unit] {
                f(element)
                if (super.decrementAndGet() == 0) {
                  continue(())
                }
              }
            }
            if (super.decrementAndGet() == 0) {
              continue(())
            }
          }
        })

    final def map[B: Manifest](f: A => B @suspendable): ContinuationizedParallelIterable[B] @suspendable =
      shift(
        new AtomicInteger(underline.size) with ((ContinuationizedParallelIterable[B] => Unit) => Unit) {
          override final def apply(continue: ContinuationizedParallelIterable[B] => Unit) {
            val results = new Array[B](super.get)
            for ((element, i) <- underline.view.zipWithIndex) {
              reset[Unit,Unit] {
                val result = f(element)
                results(i) = result
                if (super.decrementAndGet() == 0) {
                  continue(new ContinuationizedParallelIterable(results))
                }
              }
            }
          }
        })
  }

  def g: List[Int] @suspendable = List(1, 2, 3)

  def fp10: List[Int] @suspendable = {
    g.map(x => x)
  }

  def fp11: List[Int] @suspendable = {
    val z = g.map(x => x)
    z
  }

  def fp12: List[Int] @suspendable = {
    val z = List(1, 2, 3)
    z.map(x => x)
  }

  def fp20: List[Int] @suspendable = {
    g.map[Int, List[Int]](x => x)
  }

  def fp21: List[Int] @suspendable = {
    val z = g.map[Int, List[Int]](x => x)
    z
  }

  def fp22: List[Int] @suspendable = {
    val z = g.map[Int, List[Int]](x => x)(List.canBuildFrom[Int])
    z
  }

  def fp23: List[Int] @suspendable = {
    val z = g.map(x => x)(List.canBuildFrom[Int])
    z
  }

  @Test def t5506 = {
    reset[Unit,Unit] {
      assertEquals(List(1, 2, 3), fp10)
      assertEquals(List(1, 2, 3), fp11)
      assertEquals(List(1, 2, 3), fp12)
      assertEquals(List(1, 2, 3), fp20)
      assertEquals(List(1, 2, 3), fp21)
      assertEquals(List(1, 2, 3), fp22)
      assertEquals(List(1, 2, 3), fp23)
    }
  }
  class ExecutionContext

  implicit def defaultExecutionContext = new ExecutionContext

  case class Future[+T](x: T) {
    final def map[A](f: T => A): Future[A] = new Future[A](f(x))
    final def flatMap[A](f: T => Future[A]): Future[A] = f(x)
  }

  class PromiseStream[A] {
    override def toString = xs.toString

    var xs: List[A] = Nil

    final def +=(elem: A): this.type = { xs :+= elem; this }

    final def ++=(elem: Traversable[A]): this.type = { xs ++= elem; this }

    final def <<(elem: Future[A]): PromiseStream[A] @cps[Future[Any]] =
      shift { cont: (PromiseStream[A] => Future[Any]) => elem map (a => cont(this += a)) }

    final def <<(elem1: Future[A], elem2: Future[A], elems: Future[A]*): PromiseStream[A] @cps[Future[Any]] =
      shift { cont: (PromiseStream[A] => Future[Any]) => Future.flow(this << elem1 << elem2 <<< Future.sequence(elems.toSeq)) map cont }

    final def <<<(elems: Traversable[A]): PromiseStream[A] @cps[Future[Any]] =
      shift { cont: (PromiseStream[A] => Future[Any]) => cont(this ++= elems) }

    final def <<<(elems: Future[Traversable[A]]): PromiseStream[A] @cps[Future[Any]] =
      shift { cont: (PromiseStream[A] => Future[Any]) => elems map (as => cont(this ++= as)) }
  }

  object Future {

    def sequence[A, M[_] <: Traversable[_]](in: M[Future[A]])(implicit cbf: CanBuildFrom[M[Future[A]], A, M[A]], executor: ExecutionContext): Future[M[A]] =
      new Future(in.asInstanceOf[Traversable[Future[A]]].map((f: Future[A]) => f.x)(cbf.asInstanceOf[CanBuildFrom[Traversable[Future[A]], A, M[A]]]))

    def flow[A](body: => A @cps[Future[Any]])(implicit executor: ExecutionContext): Future[A] = reset[Future[A],Future[Any]](Future(body)).asInstanceOf[Future[A]]

  }

  @Test def t5538 = {
    val p = new PromiseStream[Int]
    assertEquals(Future(Future(Future(Future(Future(List(1, 2, 3, 4, 5)))))).toString, Future.flow(p << (Future(1), Future(2), Future(3), Future(4), Future(5))).toString)
  }
}
