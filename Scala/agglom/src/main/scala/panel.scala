/*
panel.scala

The window panel objects

 */

import scala.swing.*
import java.awt.image.BufferedImage


case class ImagePanel(bi: BufferedImage) extends Panel:
  override def paintComponent(g: Graphics2D) =
    g.drawImage(bi, 0, 0, null)

object ImagePanel:
  def apply(w: Int, h: Int) =
    val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val ip = new ImagePanel(bi)
    ip.preferredSize = new Dimension(w, h)
    ip

// eof

