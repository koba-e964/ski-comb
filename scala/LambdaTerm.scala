sealed abstract class LambdaTerm {
  def toString: String
  def toStringParen: String = "(" + toString + ")"
}

case class LambdaAbst(v: String, e: LambdaTerm) extends LambdaTerm {
  override def toString = "\\" + v + ". " + e.toString
}
/*
 * TODO Left-associativity of function application is not implemented. (toString yields redundant parentheses)
 */
case class LambdaApp(e1: LambdaTerm, e2: LambdaTerm) extends LambdaTerm {
  override def toString = e1.toStringParen + " " + e2.toStringParen
}
case class LambdaVar(v: String) extends LambdaTerm {
  override def toString = v
  override def toStringParen = v
}

/*
 * TODO Move main function to test files
 */

object Main {
  def main(args: Array[String]) {
    val omega = LambdaAbst("x", LambdaApp(LambdaVar("x"), LambdaVar("x")))
    println(omega)
    println(LambdaApp(omega, omega))
    println(LambdaApp(LambdaApp(omega, omega), omega))
  }
}