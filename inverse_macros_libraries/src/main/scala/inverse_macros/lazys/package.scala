package inverse_macros

package object lazys {
  @inline final def defer[A](body: => A): A@lzy = throw LazyContext(() => body)
}
