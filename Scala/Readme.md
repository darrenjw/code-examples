# Some simple Scala examples for beginners

## Tools setup

The easiest way to install all necessary Scala tools on your system is by using a tool called [Coursier](https://get-coursier.io/docs/cli-overview). See the [getting started](https://docs.scala-lang.org/getting-started/) instructions on the [Scala](https://www.scala-lang.org/) website for how to install this. Once you have Coursier installed, doing `cs setup` should install everything else that you need, including [scala-cli](https://scala-cli.virtuslab.org/) and [sbt](https://www.scala-sbt.org/). Running `cs update` regularly will keep your Scala tools up-to-date.

## Examples

In roughly increasing order of complexity:

* [e.scala](e.scala) - compute *e* from its series expansion *(for loop)*
* [prime.scala](prime.scala) - test if a given number is prime *(tail-recursive function)*
* [factor.scala](factor.scala) - compute a prime factorisation *(lists)*
* [sqrt.scala](sqrt.scala) - compute square roots with Newton's method *(nested tail-recursive function)*
* [sin.scala](sin.scala) - compute square roots with Newton's method *(non-tail-recursive function)*
* [stats.scala](stats.scala) - compute mean and sd of some numbers *(read lines from stdin)*


## Some links

* [Scala documentation](https://docs.scala-lang.org/)
* [typelevel](https://typelevel.org/)
    * [spire](https://typelevel.org/spire/)
* [breeze](https://github.com/scalanlp/breeze)

