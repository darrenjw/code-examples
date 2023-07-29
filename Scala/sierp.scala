//> using scala 3.3.0

/*
sierp.scala

Draw a Sierpinski triangle directly on a BufferedImage
Save as a PNG

scala-cli sierp.scala

*/

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D


object SierpApp:

  case class Loc(x: Double, y: Double):

    def mid(l: Loc) = Loc((x+l.x)/2, (y+l.y)/2)


  def sierp(w: Int, h: Int, l0: Loc, l1: Loc, l2: Loc, lev: Int): BufferedImage =
    val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val g2 = bi.createGraphics()
    g2.setBackground(Color.WHITE)
    g2.clearRect(0,0,w,h)
    g2.setColor(Color.RED)
    def go(l0: Loc, l1: Loc, l2: Loc, lev: Int): Unit =
      if (lev == 0)
        g2.fillPolygon(Array(l0.x, l1.x, l2.x).map(_.toInt),
          Array(l0.y, l1.y, l2.y).map(_.toInt), 3)
      else
        go(l0, l0 mid l1, l0 mid l2, lev-1)
        go(l2, l0 mid l2, l1 mid l2, lev-1)
        go(l1, l0 mid l1, l1 mid l2, lev-1)
    go(l0, l1, l2, lev)
    bi

  @main def main() =
    println("Hello")
    val bi = sierp(1000, 800, Loc(500, 0), Loc(100, 700), Loc(900, 700), 7)
    javax.imageio.ImageIO.write(bi, "png", new java.io.File("sierp.png"))
    println("Goodbye")

// eof

