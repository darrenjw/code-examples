# Some simple Scala examples for beginners

## Tools setup

The easiest way to install all necessary Scala tools on your system is by using a tool called [Coursier](https://get-coursier.io/docs/cli-overview). See the [getting started](https://docs.scala-lang.org/getting-started/) instructions on the [Scala](https://www.scala-lang.org/) website for how to install this. Once you have Coursier installed, doing `cs setup` should install everything else that you need, including [scala-cli](https://scala-cli.virtuslab.org/) and [sbt](https://www.scala-sbt.org/). Running `cs update` regularly will keep your Scala tools up-to-date.

## Examples

In roughly increasing order of complexity:

* [e.scala](e.scala) - compute *e* from its series expansion *(for loop)*
* [prime.scala](prime.scala) - test if a given number is prime *(tail-recursive function)*
* [factor.scala](factor.scala) - compute a prime factorisation *(lists)*
* [sqrt.scala](sqrt.scala) - compute square roots with Newton's method *(nested tail-recursive function)*
* [sin.scala](sin.scala) - compute sine using a trig identity *(non-tail-recursive function)*
* [stats.scala](stats.scala) - compute mean and sd of some numbers *(read lines from stdin)*
* [statse.scala](statse.scala) - compute mean and sd of some numbers *(extension methods)*
* [statsb.scala](statsb.scala) - compute mean and sd using Breeze *(Breeze scientific library)*
* [canvas1.scala](canvas1.scala) - minimal image/canvas app (set pixels) *(case classes, writing a file)*
* [canvas2.scala](canvas2.scala) - minimal image/canvas app (lines and triangles) *(folds)*
* [canvas3.scala](canvas3.scala) - minimal image/canvas app (Sierpinski triangles) *(parametrised extension)*
* [canvas4.scala](canvas4.scala) - minimal image/canvas app (circles and thick lines)
* [canvas5.scala](canvas5.scala) - minimal image/canvas app (fractal fern)
* [canvas6.scala](canvas6.scala) - minimal image/canvas app (Mandelbrot set) *(complex numbers from Spire)*
* [canvas7.scala](canvas7.scala) - minimal image/canvas app (Lorenz attractor with Euler integration) *(LazyLists)*
* [canvas8.scala](canvas8.scala) - minimal image/canvas app (save Mandelbrot as PNG) *(BufferedImage)*


## Some links

* [Scala documentation](https://docs.scala-lang.org/)
* [typelevel](https://typelevel.org/)
    * [spire](https://typelevel.org/spire/)
* [breeze](https://github.com/scalanlp/breeze)

