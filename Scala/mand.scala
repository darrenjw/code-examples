//> using scala 3.3.0
//> using dep org.typelevel::spire:0.18.0

/*
mand.scala

Draw a Mandelbrot set directly onto a BufferedImage
Save as a PNG

scala-cli mand.scala

 */

import spire.*
import spire.math.*
import spire.implicits.*

import java.awt.image.BufferedImage

object MandelApp:

  def level(c: Complex[Double], maxIts: Int): Int =
    @annotation.tailrec
    def go(z: Complex[Double], its: Int): Int =
      if (its >= maxIts) -1
      else
        val zn = z * z + c
        if (zn.abs > 2.0)
          its
        else
          go(zn, its + 1)
    go(Complex(0.0, 0.0), 1)

  def mand(
      w: Int,
      h: Int,
      tl: Complex[Double],
      iRange: Double,
      maxIts: Int
  ): BufferedImage =
    val res = iRange.toDouble / h
    val canvas = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val wr = canvas.getRaster
    for (x <- 0 until w)
      for (y <- 0 until h)
        val c = tl + Complex(x * res, -y * res)
        val l = level(c, maxIts)
        if (l == -1)
          wr.setSample(x, y, 2, 20)
        else
          wr.setSample(x, y, 0, 255 * l / maxIts)
    canvas

  @main def main() =
    println("Hello")
    val bi = mand(1000, 800, Complex(-2.5, 1.5), 3.0, 60)
    javax.imageio.ImageIO.write(bi, "png", new java.io.File("mand.png"))
    println("Goodbye")

// eof
