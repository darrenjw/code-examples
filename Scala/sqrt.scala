//> using scala 3.3.0

/*
sqrt.scala
Compute square roots with Newton's method
https://en.wikipedia.org/wiki/Newton%27s_method

Run with:
scala-cli sqrt.scala -- 2.0

 */

object SqrtApp:

  @main
  def sqrtMain(x: Double) =
    print(s"sqrt($x) = ")
    println(mySqrt(x))

  def mySqrt(x: Double): Double =
    @annotation.tailrec
    def go(sx: Double): Double =
      if (math.abs(sx * sx - x) < 1.0e-8)
        sx
      else
        go((sx + x / sx) / 2)
    go(x)

// eof
