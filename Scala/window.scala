//> using scala 3.3.0
//> using dep org.typelevel::spire:0.18.0
//> using dep org.scala-lang.modules::scala-swing:3.0.0

/*
window.scala

Draw a Mandelbrot set directly onto a BufferedImage
then render in a window on the console

scala-cli window.scala

 */

import spire.*
import spire.math.*
import spire.implicits.*

import java.awt.image.BufferedImage

import scala.swing.*

object MandelApp extends SimpleSwingApplication:

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

  def top = new MainFrame {
    println("Hello")
    title = "Scala Swing Application Window (Mandelbrot set)"
    val panel = ImagePanel(1000, 800)
    contents = new BoxPanel(Orientation.Vertical) {
      contents += panel
      border = Swing.EmptyBorder(20, 20, 20, 20)
    }
    println("Goodbye")
  }

  case class ImagePanel(bi: BufferedImage) extends Panel:
    override def paintComponent(g: Graphics2D) =
      g.drawImage(bi, 0, 0, null)

  object ImagePanel:
    def apply(w: Int, h: Int) =
      val bi = mand(w, h, Complex(-2.5, 1.5), 3.0, 60)
      val ip = new ImagePanel(bi)
      ip.preferredSize = new Dimension(w, h)
      ip

// eof
