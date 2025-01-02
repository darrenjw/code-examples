//> using scala 3.3.0

/*
statse.scala

Compute mean and SD of some numbers entered at command line
Using extension methods

Run with (eg.):
scala-cli statse.scala -- 5 < ../C/fiveNumbers.txt

 */

import scala.io.StdIn.readLine

object StatsApp:

  @main
  def statsMain(n: Int) =
    println(s"Enter $n numbers:")
    val v = Vector.tabulate(n)(i => readLine().toDouble)
    val m = v.mean
    println(m)
    println(v.sd(m))

  extension (v: Vector[Double]) def mean = v.sum / v.length

  extension (v: Vector[Double])
    def sd(m: Double) =
      val ss = v.foldLeft(0.0)((s, xi) => s + (xi - m) * (xi - m))
      math.sqrt(ss / (v.length - 1))

// eof
