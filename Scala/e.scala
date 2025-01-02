//> using scala 3.3.0

/*
e.scala
Compute e from its series expansion:
e = 1/0! + 1/1! + 1/2! + 1/3! + ...

Run with:
scala-cli e.scala

 */

object EApp:

  @main def eRun() =
    var e = 1.0
    var denom = 1
    for i <- 1 to 20
    do
      denom = denom * i
      e = e + 1.0 / denom
      println(s"$i: $e")

// eof
