package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    var m = Map[String, Signal[Double]]()
    for( (k, v) <- namedExpressions) {
      val result = eval(v(), namedExpressions)
      m = m + (k -> Var(result))
    }
    m
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def evalHelper(expr: Expr, references: Map[String, Signal[Expr]], fromRef: Set[String]) : Double = {
      expr match {
        case Literal(v) => v
        case Plus(a, b) => evalHelper(a, references, fromRef) + evalHelper(b, references, fromRef)
        case Minus(a, b) => evalHelper(a, references, fromRef) - evalHelper(b, references, fromRef)
        case Times(a, b) => evalHelper(a, references, fromRef) * evalHelper(b, references, fromRef)
        case Divide(a, b) => evalHelper(a, references, fromRef) / evalHelper(b, references, fromRef)
        case Ref(name) =>
          if(fromRef(name))
            Double.NaN
          else
            evalHelper(getReferenceExpr(name, references), references, fromRef + name)
      }
    }

    evalHelper(expr, references, Set[String]())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
