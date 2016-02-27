object Main {
  import java.io._
  import java.nio.file._
  def main(args: Array[String]) {
    if (args.length >= 1) {
      val infile = args(0)
      val outfile = if (args.length >= 2) args(1) else infile + ".out"
      val dest = new BufferedWriter(new FileWriter(new File(outfile)))
      val encoded = Files.readAllBytes(Paths.get(infile)) 
      val content = new String(encoded, java.nio.charset.StandardCharsets.UTF_8)
      val skiTerm: SKI.Expr = SKICompiler.compile(new LambdaParser().parse(content))
      dest.write(SKIEmit.emit(skiTerm))
      dest.close
    } else {
      println("java Main [filename] ([outfile])")
    }
  }
}