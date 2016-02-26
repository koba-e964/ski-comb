import scala.util.parsing.combinator.RegexParsers

/**
 * <Expr> ::= \<Var>. <Expr> | <appExpr>
 * <appExpr> ::= <appExpr> <atomicExpr> | <atomicExpr>
 * <atomicExpr> ::= (<Expr>) | <Var>
 */

class LambdaParser extends RegexParsers {
  def expr: Parser[LambdaTerm] 
    = appExpr ^^ {ls => ls reduceLeft ((u, v) => LambdaApp(u, v)) } |
     ("\\" ~ vari ~ "." ~ expr) ^^ { case _ ~ x ~ _ ~ e => LambdaAbst(x, e) }
  def appExpr: Parser[List[LambdaTerm]]
    = (atomicExpr ~ appExpr) ^^ { case x ~ y => x :: y } |
     atomicExpr ^^ { x => List(x) }
  def atomicExpr: Parser[LambdaTerm]
    = vari ^^ { x => LambdaVar(x) } |
     ("(" ~ expr ~ ")") ^^ { case _ ~ e ~ _ => e }
  def vari: Parser[String] = """[a-zA-Z_]\w*""".r
  def parse(s: String): LambdaTerm = {
    parseAll(expr, s) match {
      case Success(r, in) => r
      case fail => scala.sys.error(fail.toString)
    }
  }
}

object LambdaParserTest {
  def test(s: String, t: LambdaTerm) {
    val lp = new LambdaParser
    val term = lp.parse(s)
    if (t == term) {
      println("[ok] " + t)
    } else {
      println("[err] " + t + " but got: " + term)
    }
    
  }
  def main(args: Array[String]) {
    val vx = LambdaVar("x")
    val vy = LambdaVar("y")
    val vid = LambdaVar("id")
    test("\\x. x y", LambdaAbst("x", LambdaApp(vx, vy)))
    test("(\\id. id id)(\\x. x)", LambdaApp(LambdaAbst("id", LambdaApp(vid, vid)), LambdaAbst("x", vx)))
  }
}