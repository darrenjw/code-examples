//> using scala 3.3.0

/*
factor.scala
Compute prime factorisation

Run with:
scala-cli factor.scala -- 99

*/

import scala.util.boundary, boundary.break

object FactorApp:

  @main def factorMain(n: Int) =
    print(s"$n = ")
    factor(n)

  def factor(n: Int): Unit =
    boundary:
      for
        i <- 2 until n
      do
        if (n % i == 0)
          print(s"$i x ")
          factor(n / i)
          break()
      println(n)

// eof

