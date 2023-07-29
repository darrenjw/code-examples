/*
agglom.scala

Simple scala swing app to show an agglomeration process in a window on the console

*/

import java.awt.image.{BufferedImage, WritableRaster}
import java.awt.Color
import scala.swing.*


object AgglomApp extends SimpleSwingApplication:

  val w = 1000
  val h = 800

  val rng = scala.util.Random()

  def top = new MainFrame {
    println("Hello")
    title = "Agglomeration process"
    val panel = ImagePanel(w, h)
    contents = new BoxPanel(Orientation.Vertical) {
      contents += panel
      border = Swing.EmptyBorder(20, 20, 20, 20)
    }
    val wr = panel.bi.getRaster
    val timer = new javax.swing.Timer(1, Swing.ActionListener(e =>
      update(wr)
      //println("Updated...")
      panel.repaint()
    ))
    val g2 = panel.bi.createGraphics
    g2.setBackground(Color.WHITE)
    g2.clearRect(0, 0, w, h)
    wr.setSample(w/2, h-1, 0,  0) // dark green pixel
    wr.setSample(w/2, h-1, 1, 20) // dark green pixel
    wr.setSample(w/2, h-1, 2,  0) // dark green pixel
    timer.start()
  }

  case class ImagePanel(bi: BufferedImage) extends Panel:
    override def paintComponent(g: Graphics2D) =
      g.drawImage(bi, 0, 0, null)

  object ImagePanel:
    def apply(w: Int, h: Int) =
      val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
      val ip = new ImagePanel(bi)
      ip.preferredSize = new Dimension(w, h)
      ip

  def update(wr: WritableRaster): Unit =
    val x0 = rng.nextInt(w)
    val y0 = rng.nextInt(h)
    @annotation.tailrec
    def go(x: Int, y: Int): (Int, Int) =
      if (isAdjacent(wr, x, y))
        (x, y)
      else
        val dir = rng.nextInt(4)
        dir match
          case 0 => go(x, math.max(0,y-1))
          case 1 => go(math.max(0,x-1), y)
          case 2 => go(math.min(w-1,x+1), y)
          case 3 => go(x, math.min(h-1,y+1))
    val (x, y) = go(x0, y0)
    //println(s"MATCH: $x $y")
    wr.setSample(x, y, 0,  0) // dark green pixel
    wr.setSample(x, y, 1, 20) // dark green pixel
    wr.setSample(x, y, 2,  0) // dark green pixel

  def isAdjacent(wr: WritableRaster, x: Int, y: Int): Boolean =
    isOccupied(wr, x, y-1)|
    isOccupied(wr, x-1, y)|
    isOccupied(wr, x+1, y)|
    isOccupied(wr, x, y+1)

  def isOccupied(wr: WritableRaster, x: Int, y: Int): Boolean =
    (x>=0)&&(y>=0)&&(x<w)&&(y<h)&&(wr.getSample(x, y, 0) == 0)

// eof

