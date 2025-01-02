//> using scala 3.3.0

/*
stats.scala

Compute mean and SD of some numbers entered at command line

Run with (eg.):
scala-cli stats.scala -- 5 < ../C/fiveNumbers.txt

 */

import scala.io.StdIn.readLine

object StatsApp:

  @main
  def statsMain(n: Int) =
    println(s"Enter $n numbers:")
    val v = Vector.tabulate(n)(i => readLine().toDouble)
    val m = mean(v)
    println(m)
    println(sd(v, m))

  def mean(v: Vector[Double]) = v.sum / v.length

  def sd(v: Vector[Double], m: Double) =
    val ss = v.foldLeft(0.0)((s, xi) => s + (xi - m) * (xi - m))
    math.sqrt(ss / (v.length - 1))

// eof
