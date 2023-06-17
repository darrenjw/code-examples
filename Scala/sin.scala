//> using scala 3.3.0

/*
sin.scala
Compute sin using the trig identity:

sin(x) = 3sin(x/3) - 4sin^3(x/3)

and the fact that for small enough x, sin(x) ~= x.

Run with:
scala-cli sin.scala -- 1.0

*/


object SqrtApp:

  @main
  def sinMain(x: Double) =
    print(s"sin($x) = ")
    println(mySin(x))

  def mySin(x: Double): Double =
    if math.abs(x) < 1.0e-5 then
      x
    else
      val sx3 = mySin(x/3)
      3*sx3 - 4*sx3*sx3*sx3


// eof

