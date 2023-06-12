//> using scala 3.3.0

/*
prime.scala
Test if a given number is prime

Run with:
scala-cli prime.scala -- 101

*/

object PrimeApp:

  @main
  def prime(n: Int) =
    println(n)
    val result = if isPrime(n) then "Prime" else "Not prime"
    println(result)

  @annotation.tailrec
  def isPrime(n: Int, from: Int = 2): Boolean =
    if (from*from > n)
      true
    else
      n % from match
      case 0 => false
      case _ => isPrime(n, from + 1)

// eof

