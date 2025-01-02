//> using scala 3.3.0

/*
factor.scala
Compute prime factorisation

Run with:
scala-cli factor.scala -- 99

 */

object FactorApp:

  @main
  def factorMain(n: Int) =
    print(s"$n = ")
    println(factor(n).mkString(" x "))

  def factor(n: Int, from: Int = 2): List[Int] =
    if (from * from > n)
      List(n)
    else
      n % from match
        case 0 => from :: factor(n / from, from)
        case _ =>
          factor(n, from + 1)

// eof
