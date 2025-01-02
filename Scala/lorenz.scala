//> using scala 3.3.0
//> using dep org.scalanlp::breeze:2.1.0
//> using dep org.scalanlp::breeze-viz:2.1.0

/*
lorenz.scala

Use Breeze to integrate the Lorenz system and display the results in
a window on the console

https://en.wikipedia.org/wiki/Lorenz_system

scala-cli lorenz.scala

 */

import breeze.linalg.*
import breeze.numerics.*
import breeze.integrate.*
import breeze.plot.*

type DVD = DenseVector[Double]

object Lorenz:

  @main
  def run() =
    println("Lorenz")
    val rhs = lorenzRhs(28.0, 10.0, 8.0 / 3.0)
    val ode = new HighamHall54Integrator(0.0001, 0.1)
    val res = ode.integrate(
      (x, t) => rhs(x),
      DenseVector(1.0, 0.5, 0.1),
      linspace(0, 100, 100000).toArray
    )
    val fig = Figure("Lorenz attractor")
    fig.width = 800
    fig.height = 600
    val p = fig.subplot(1, 1, 0)
    p += plot(res map (_(0)), res map (_(1)))
    fig.refresh()
    fig.saveas("lorenz.pdf")

  def lorenzRhs(rho: Double, sigma: Double, beta: Double)(v: DVD): DVD =
    DenseVector(
      sigma * (v(1) - v(0)),
      v(0) * (rho - v(2)) - v(1),
      v(0) * v(1) - beta * v(2)
    )

// eof
