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
class LambdaLexer extends StdLexical with ImplicitConversions {
  case object LAMBDA extends Token {
    override def chars = scala.sys.error("LAMBDA")
  }
}

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
  def main(args: Array[String]) {
    val lex = new LambdaLexer
    val scn = new lex.Scanner("""
      \x. x x
      """)
  }
}