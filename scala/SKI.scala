object SKI {
  sealed abstract class Expr {
    def toString: String
    def toStringParen: String = toString
    def size: Int = 1 // the number of combinators this term contains
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
    override def size = e1.size + e2.size
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
  def comp: Expr = EApp(S, EApp(EApp(K, S), K))
  def churchInt(v: Int): Expr = v match {
    case 0 => EApp(S, K)
    case 1 => I // optimization, S (S (K S) K) (S K) -> I
    case _ if v < 0 => K // undefined
    case _ => EApp(churchSucc, churchInt(v - 1))
  }
  def churchSucc: Expr = EApp(S, EApp(EApp(S, EApp(K, S)), K))
}
object SKITest {
  import SKI._
  def main(args: Array[String]) {
    val omega = EApp(EApp(S, I), I)
    val omegaI = EApp(omega, I)
    val omegaX = EApp(omega, EVar("x"))
    println("omega id = " + omegaI)
    println("->^\\beta " + evaluate(omegaI))
    println("omega x = " + omegaX)
    println("->^\\beta " + evaluate(omegaX))
    val churchTwo = churchInt(2)
    println("2 f x --> " + evaluate(EApp(EApp(churchTwo, EVar("f")), EVar("x"))))
  }
}