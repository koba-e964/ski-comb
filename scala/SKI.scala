object SKI {
  sealed abstract class Expr {
    def toString: String
    def toStringParen: String = toString
    def size: Int = 1 // the number of combinators this term contains
    def freeVars: Set[String] = Set()
    def isClosed: Boolean = freeVars.isEmpty
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
    override def freeVars = e1.freeVars ++ e2.freeVars
  }
  case class EVar(v: String) extends Expr {
    override def toString = v
    override def freeVars = Set(v)
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
  def comp: Expr = EApp(EApp(S, EApp(K, S)), K)
  /**
   * Church-encode an integer n to \f x. f^n x.
   */
  def churchInt(v: Int): Expr = v match {
    case 0 => EApp(K, I)
    //case 1 => I // optimization, S (S (K S) K) (S K) -> I
    case _ if v < 0 => K // undefined
    case _ => EApp(churchSucc, churchInt(v - 1))
  }
  def churchSucc: Expr = EApp(S, comp)
  /**
   * Returns the integer that the given Church-encoded expression represents.
   */
  def exprToInt(e: Expr): Int = {
    // TODO check e is closed
    val f = "undefinedF"
    val x = "undefinedX"
    val fnx = evaluate(EApp(EApp(e, EVar(f)), EVar(x)))
    fnx.size - 1 // |fnx| = |f^n x| = n + 1
  }
  /*
   * Useful SKI terms.
   */
  /**
   * Self-application.
   */
  def omega = EApp(EApp(S, I), I)
  /**
   * Chris Baker's iota combinator. iota := \x. x S K
   */
  def iota = EApp(EApp(S, EApp(EApp(S, I), EApp(K, S))), EApp(K, K))

}
object SKITest {
  import SKI._
  def testEval(term: Expr, caption: String) {
    println(caption + term)
    println("->^\\beta " + evaluate(term))
  }
  def testChurchUnchurch(i: Int) {
    val uci = exprToInt(churchInt(i))
    if (i == uci) {
      println("exprToInt(churchInt(" + i +")) = " + uci + " [ok]")
    } else {
      println("exprToInt(churchInt(" + i +")) = " + uci + " [err]")
    }
  }
  def main(args: Array[String]) {
    val omegaI = EApp(omega, I)
    val omegaX = EApp(omega, EVar("x"))
    testEval(omegaI, "omega id = ")
    testEval(omegaX, "omega x = ")
    val churchTwo = churchInt(2)
    testEval(EApp(EApp(churchTwo, EVar("f")), EVar("x")), "2 f x =")
    println("iota x ---> " + evaluate(EApp(iota, EVar("x"))))
    for (i <- 0 to 5) {
      testChurchUnchurch(i)
    }
  }
}