/*
agglom.scala

Simple scala swing app to show an agglomeration process in a window on the console

*/

import scala.swing.*
import java.awt.Color


object Global:
  val w = 1000
  val h = 800
  val rng = scala.util.Random()


object AgglomApp extends SimpleSwingApplication:

  import Global.*
  import Logic.update

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
      panel.repaint()
    ))
    val g2 = panel.bi.createGraphics
    g2.setBackground(Color.WHITE)
    g2.clearRect(0, 0, w, h)
    (w/4 until 3*w/4).foreach(x =>
      wr.setSample(x, h-1, 0,  0) // dark green pixel
      wr.setSample(x, h-1, 1, 20) // dark green pixel
      wr.setSample(x, h-1, 2,  0) // dark green pixel
    )
    timer.start()
  }

// eof

