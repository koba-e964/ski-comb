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
  type Env = Map[String, (String, IR)]
  type MuEnv = mu.Map[String, (String, IR)]
  sealed abstract class IR {
    
  }
  case object IRS extends IR
  case object IRK extends IR
  case object IRI extends IR
  case class IRApp(e1: IR, e2: IR) extends IR {
    
  }
  case class IRVar(name: String) extends IR
  case class IRGlobal(name: String) extends IR
  
  def lambdaLift(e: LambdaTerm): (IR, Env) = {
    cnt = 0
    env.clear
    val lli = llIntern(e)
    (lli, env.toMap)
  }
  def llIntern(e: LambdaTerm): IR = e match {
    case LambdaAbst(x, t) => {
      /* add env */
      val irt = llIntern(t)
      val name = freshName
      env += name -> (x -> irt)
      IRGlobal(name)
    }
    case LambdaApp(t1, t2) => IRApp(llIntern(t1), llIntern(t2))
    case LambdaVar(x) => IRVar(x)
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
    println(sc.lambdaLift(omega))
    println(sc.lambdaLift(comp))
    println(sc.lambdaLift(ycomb))
  }
}