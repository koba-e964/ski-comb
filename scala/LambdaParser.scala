import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh

/**
 * <Expr> ::= \<Var>. <Expr> | <AppExpr>
 * <AppExpr> ::= <AppExpr> <AtomicExpr> | <AtomicExpr>
 * <AtomicExpr> ::= (<Expr>) | <Var>
 * 
 */

class LambdaParser extends JavaTokenParsers {
  def expr: Parser[LambdaTerm] 
    = appexpr ^^ {x => x reduceLeft ((u: LambdaTerm, v: LambdaTerm) => LambdaApp(u, v)) } |
     ("\\" ~ vari ~ "." ~ expr) ^^ { case _ ~ x ~ _ ~ e => LambdaAbst(x, e) }
  def appexpr: Parser[List[LambdaTerm]]
    = (atomicexpr ~ appexpr) ^^ { case x ~ y => x :: y } |
     atomicexpr ^^ { x => List(x) }
  def atomicexpr: Parser[LambdaTerm]
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