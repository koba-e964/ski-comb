object SKI {
  sealed abstract class Expr {
    def toString: String
    def toStringParen: String = toString
  }
  case object S extends Expr {
    override def toString = "S"
  }
  case object K extends Expr {
    override def toString = "K"
  }
  case object I extends Expr {
    override def toString = "I"
  }
  case class EApp(e1: Expr, e2: Expr) extends Expr {
    override def toString = e1.toString + " " + e2.toStringParen
    override def toStringParen = "(" + toString + ")"
  }
  case class EVar(v: String) extends Expr {
    override def toString = v
  }
  def oneStepReducible(e: Expr): Boolean = e match {
    case EApp(EApp(EApp(S, _), _), _) => true
    case EApp(S, EApp(EApp(K, _), I)) => true
    case EApp(EApp(K, _), _) => true
    case EApp(I, _) => true
    case EApp(x, y) => oneStepReducible(x) || oneStepReducible(y)
    case _ => false
  }
  def cbnReduceOnce(e : Expr): Expr = e match {
    case EApp(EApp(EApp(S, x), y), z) => EApp(EApp(x, z), EApp(y, z))
    case EApp(EApp(K, x), _) => x
    case EApp(I, x) => x
    case EApp(x, y) => if (oneStepReducible(x)) EApp(cbnReduceOnce(x), y) else EApp(x, cbnReduceOnce(y))
    case x => x
  }
  /**
   * Evaluate to a normal form
   */
  def evaluate(e: Expr): Expr = {
    var v = e
    while (oneStepReducible(v)) {
      v = cbnReduceOnce(v)
    }
    v
  }
}
object SKITest {
  import SKI._
  def main(args: Array[String]) {
    val omega = EApp(EApp(S, I), I)
    val omegaI = EApp(omega, I)
    println("omega id = " + omegaI)
    println("->^\\beta " + evaluate(omegaI))
  }
}