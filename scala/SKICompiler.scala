import scala.collection.{mutable => mu}

object SKICompiler {
  type Env = Map[String, (List[String], IR)]
  sealed abstract class IR {
    def toString: String
    def toStringParen: String = toString
    def freeVars: Set[String] = Set()
  }
  case class IRApp(e1: IR, e2: IR) extends IR {
    override def toString = e1.toString + " " + e2.toStringParen
    override def toStringParen = "(" + toString + ")"
    override def freeVars = e1.freeVars ++ e2.freeVars
  }
  case class IRVar(name: String) extends IR {
    override def toString = name
    override def freeVars = Set(name)
  }
  case class IRGlobal(name: String) extends IR {
    override def toString = name
  }
  /* TODO impose e is closed, and bound variables are distinct */
  def compile(e: LambdaTerm): SKI.Expr = {
    val (ir, env) = lambdaLift(e)
    convertToSKI(ir, env)
  }
  def lambdaLift(e: LambdaTerm): (IR, Env) = {
    val conv = new LLConverter
    val lli = conv.llIntern(e)
    (lli, conv.env.toMap)
  }
  def convertToSKI(ir: IR, env: Env): SKI.Expr = {
    val conv = new IRConverter(env)
    irToSKI(conv.convInternal(List(), ir))
  }
  def irToSKI(ir: IR): SKI.Expr = ir match {
    case IRGlobal(x) => x match {
      case "S" => SKI.S
      case "K" => SKI.K
      case "I" => SKI.I
      case _ => scala.sys.error("unexpected combinator in irToSKI")
    }
    case IRApp(t1, t2) => SKI.EApp(irToSKI(t1), irToSKI(t2))
    case IRVar(_) => scala.sys.error("unexpected var, the given ir is not closed")
  }
  private[this] final class LLConverter {
    type MuEnv = mu.Map[String, (List[String], IR)]
    var cnt: Int = 0 // counter
    val env: MuEnv = mu.Map()
    private[this] def freshName: String = {
      cnt += 1
      "G" + cnt
    }
    def llIntern(e: LambdaTerm): IR = e match {
      case LambdaAbst(x, t) => {
      /* add env */
      val irt = llIntern(t)
      val name = freshName
      val freeVars: List[String] = (t.freeVars - x).toList
      env += name -> ((freeVars :+ x) -> irt)
      freeVars.foldLeft(IRGlobal(name): IR)((x, y) => IRApp(x, IRVar(y)))
      }
      case LambdaApp(t1, t2) => IRApp(llIntern(t1), llIntern(t2))
      case LambdaVar(x) => IRVar(x)
    }
  }
  private[this] class IRConverter(env: Env) {
    private[this] val memo: mu.Map[String, IR] = mu.Map() // memoization
    def convInternal(args: List[String], e: IR): IR = {
      val et = elimGlobal(e)
      if (args.isEmpty) et else convInternal(args.init, convInternalOne(args.last, et))
    }
    val s = IRGlobal("S")
    val k = IRGlobal("K")
    val i = IRGlobal("I")
    def convInternalOne(arg: String, e: IR): IR = e match {
      case _ if (e.freeVars(arg) == false) => IRApp(k, e) // optimization
      case IRApp(e1, IRVar(x)) if (x == arg && e1.freeVars(arg) == false) =>
      e1 // optimization, not necessary
      case IRApp(e1, e2) => IRApp(IRApp(s, convInternalOne(arg, e1)), convInternalOne(arg, e2))
      case IRVar(x) => if (x == arg) i else IRApp(k, IRVar(x))
      case IRGlobal(g) => g match {
        case "S" => IRApp(k, s)
        case "K" => IRApp(k, k)
        case "I" => IRApp(k, i)
        case _ => scala.sys.error("global combinator, which is not expected")
      }
    }
    def elimGlobal(e: IR): IR = e match {
      case IRGlobal(g) => g match {
        case "S" => e
        case "K" => e
        case "I" => e
        case _ =>
          if (!(memo contains g)) {
            val (a, t) = env(g)
            memo(g) = convInternal(a, t)
          }
          memo(g)
      }
      case IRApp(t1, t2) => IRApp(elimGlobal(t1), elimGlobal(t2))
      case _ => e
    }
  }
}

object SKICompilerTest {
  import SKICompiler._
  def main(args: Array[String]) {
    val omega = LambdaAbst("x", LambdaApp(LambdaVar("x"), LambdaVar("x")))
    val comp = LambdaAbst("f", LambdaAbst("g", LambdaAbst("x",
        LambdaApp(LambdaVar("f"), LambdaApp(LambdaVar("g"), LambdaVar("x"))))))
    val vx = LambdaVar("x")
    val xfxx = LambdaAbst("x", LambdaApp(LambdaVar("f"), LambdaApp(vx, vx)))
    val ycomb = LambdaAbst("f", LambdaApp(xfxx, xfxx))
    println("omega = " + compile(omega))
    println("comp = " + compile(comp))
    println("ycomb = " + compile(ycomb))
  }
}