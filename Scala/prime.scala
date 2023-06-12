//> using scala 3.3.0

/*
prime.scala
Test if a given number is prime

Run with:
scala-cli prime.scala -- 101

*/

object PrimeApp:

  @main def prime(n: Int) =
    println(n)
    val result = if isPrime(n) then "Prime" else "Not prime"
    println(result)

  def isPrime(n: Int): Boolean =
    val hasFactor = (2 until n).map(i => (n % i) == 0).reduce(_|_)
    !hasFactor

// eof

