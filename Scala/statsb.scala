//> using scala 3.3.0
//> using dep org.scalanlp::breeze:2.1.0

/*
stats.scala

Compute mean and SD of some numbers entered at command line
Using Breeze

Run with (eg.):
scala-cli statsb.scala -- 5 < ../C/fiveNumbers.txt

*/

import scala.io.StdIn.readLine
import breeze.linalg.*
import breeze.numerics.*
import breeze.stats.*

object StatsApp:

  @main
  def statsbMain(n: Int) =
    println(s"Enter $n numbers:")
    val v = DenseVector.tabulate[Double](n)(i => readLine().toDouble)
    val mv = meanAndVariance(v)
    println(mv.mean)
    println(math.sqrt(mv.variance))




// eof

