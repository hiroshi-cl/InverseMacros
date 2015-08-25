import language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object debug {

  private[this] class Bundle(val c: Context) {

    import c.universe._

    def appliedImpl(prefix: Tree)(name: Tree)(a: Tree*) =
      catchError(macroApply(prefix)(name)(a: _*))

    def showImpl(t: Tree) = q"${c.universe.show(t)}"

    def showRawImpl(t: Tree) = q"${c.universe.showRaw(t)}"

    def parseImpl(code: Tree) = {
      val Literal(Constant(s: String)) = code
      catchError(c.parse(s))
    }

    def macroApply(prefix: Tree)(name: Tree)(a: Tree*) = {
      val Literal(Constant(s: String)) = name
      c.typecheck(q"$prefix.${TermName(s).encodedName.toTermName}(..$a)")
    }

    def catchError(tree: => Tree) = {
      try {
        c.typecheck(tree)
      } catch {
        case e: Throwable =>
          c.typecheck(q"throw new Exception(${e.toString})")
      }
    }
  }

  def applied(prefix: Any)(name: String)(a: Any*): Any = macro Bundle.appliedImpl

  def show(t: Any): String = macro Bundle.showImpl

  def showRaw(t: Any): String = macro Bundle.showRawImpl

  def parse(code: String): Any = macro Bundle.parseImpl

  private[this] val pat = java.util.regex.Pattern.compile(java.util.regex.Pattern.quote("fresh$macro$") + "\\d+")

  def replaceFreshVariables(code: String): String = pat.matcher(code).replaceAll("\\$\\$")

  def expectException(a: => Any): Boolean = try {
    a
    false
  } catch {
    case e: Exception =>
      true
  }
}
