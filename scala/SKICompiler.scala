import scala.collection.{mutable => mu}

/**
 * This is a class rather than an object, because it has states.
 */
class SKICompiler {
  var cnt: Int = 0 // counter
  val env: MuEnv = mu.Map()
  private[this] def freshName: String = {
    cnt += 1
    "G" + cnt
  }
  type Env = Map[String, (List[String], IR)]
  type MuEnv = mu.Map[String, (List[String], IR)]
  sealed abstract class IR {
    def toString: String
    def toStringParen: String = toString
  }
  case class IRApp(e1: IR, e2: IR) extends IR {
    override def toString = e1.toString + " " + e2.toStringParen
    override def toStringParen = "(" + toString + ")"
  }
  case class IRVar(name: String) extends IR {
    override def toString = name
  }
  case class IRGlobal(name: String) extends IR {
    override def toString = name
  }
  def compile(e: LambdaTerm): SKI.Expr = {
    val (ir, env) = lambdaLift(e)
    convertToSKI(ir, env)
  }
  def lambdaLift(e: LambdaTerm): (IR, Env) = {
    cnt = 0
    env.clear
    val lli = llIntern(e)
    (lli, env.toMap)
  }
  def convertToSKI(ir: IR, env: Env): SKI.Expr = ir match {
    case IRGlobal(name) =>
      val (u, v) = env(name)
      irToSKI(convInternal(u, v, env))
    case IRApp(i1, i2) => SKI.EApp(convertToSKI(i1, env), convertToSKI(i2, env))
    case IRVar(x) => scala.sys.error("The given ir is not closed!!")
  }
  def irToSKI(ir: IR): SKI.Expr = ir match {
    case IRGlobal(x) => x match {
      case "S" => SKI.S
      case "K" => SKI.K
      case "I" => SKI.I
    }
    case IRApp(t1, t2) => SKI.EApp(irToSKI(t1), irToSKI(t2))
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
  def convInternal(args: List[String], e: IR, env: Env): IR = {
    if (args.isEmpty) e else convInternal(args.init, convInternalOne(args.last, e, env), env)
  }
  val s = IRGlobal("S")
  val k = IRGlobal("K")
  val i = IRGlobal("I")
  def convInternalOne(arg: String, e: IR, env: Env): IR = e match {
    case IRApp(e1, e2) => IRApp(IRApp(s, convInternalOne(arg, e1, env)), convInternalOne(arg, e2, env))
    case IRVar(x) => if (x == arg) i else IRApp(k, IRVar(x))
    case IRGlobal(g) => g match {
      case "S" => IRApp(k, s)
      case "K" => IRApp(k, k)
      case "I" => IRApp(k, i)
      case _ => val (a, t) = env(g)
      convInternalOne(arg, convInternal(a, t, env), env)
    }
  }
}

object SKICompilerTest {
  def main(args: Array[String]) {
    val omega = LambdaAbst("x", LambdaApp(LambdaVar("x"), LambdaVar("x")))
    val comp = LambdaAbst("f", LambdaAbst("g", LambdaAbst("x",
        LambdaApp(LambdaVar("f"), LambdaApp(LambdaVar("g"), LambdaVar("x"))))))
    val vx = LambdaVar("x")
    val xfxx = LambdaAbst("x", LambdaApp(LambdaVar("f"), LambdaApp(vx, vx)))
    val ycomb = LambdaAbst("f", LambdaApp(xfxx, xfxx))
    val sc = new SKICompiler
    println("omega = " + sc.compile(omega))
    println("comp = " + sc.compile(comp))
    println("ycomb = " + sc.compile(ycomb))
  }
}