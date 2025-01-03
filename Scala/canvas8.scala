//> using scala 3.3.0
//> using dep org.typelevel::spire:0.18.0

/*
canvas8.scala

Simple image/canvas app drawing a Mandelbrot set
Saves to a PNG via a BufferedImage

scala-cli canvas8.scala

 */

import spire.*
import spire.math.*
import spire.implicits.*

import java.awt.image.BufferedImage

case class Colour(r: Int, g: Int, b: Int)

case class Loc(x: Int, y: Int):

  def mid(l: Loc) = Loc((x + l.x) / 2, (y + l.y) / 2)

case class Image[T](w: Int, h: Int, data: Vector[T]):

  def apply(l: Loc): T = data(l.x * h + l.y)

  def map[S](f: T => S): Image[S] = Image(w, h, data map f)

  def updated(l: Loc, value: T): Image[T] =
    if ((l.x >= 0) & (l.y >= 0) & (l.x < w) & (l.y < h))
      Image(w, h, data.updated(l.x * h + l.y, value))
    else this

  def line(l0: Loc, l1: Loc, c: T): Image[T] =
    val xd = math.abs(l1.x - l0.x)
    val yd = math.abs(l1.y - l0.y)
    val n = math.max(xd, yd) + 1
    val is = (0 to n).toList
    val x = is.map(i =>
      math.round(l0.x.toDouble * (n - i) / n + l1.x.toDouble * i / n).toInt
    )
    val y = is.map(i =>
      math.round(l0.y.toDouble * (n - i) / n + l1.y.toDouble * i / n).toInt
    )
    val ls = (x zip y) map (p => Loc(p._1, p._2))
    ls.foldLeft(this)((im, li) => im.updated(li, c))

  def tri(l0: Loc, l1: Loc, l2: Loc, c: T): Image[T] =
    val sorted = List(l0, l1, l2).sortWith(_.y < _.y)
    val iTop = 0 until (sorted(1).y - sorted(0).y)
    val linesTop = iTop.map(i =>
      (
        sorted(0).y + i,
        sorted(2).x.toDouble * i / (sorted(2).y - sorted(0).y) +
          sorted(0).x.toDouble * (sorted(2).y - sorted(0).y - i) / (sorted(
            2
          ).y - sorted(0).y),
        sorted(1).x.toDouble * i / (sorted(1).y - sorted(0).y) +
          sorted(0).x.toDouble * (sorted(1).y - sorted(0).y - i) / (sorted(
            1
          ).y - sorted(0).y)
      )
    )
    val iBot = 0 until (sorted(2).y - sorted(1).y)
    val linesBot = iBot.map(i =>
      (
        sorted(1).y + i,
        sorted(2).x.toDouble * (sorted(1).y - sorted(0).y + i) / (sorted(
          2
        ).y - sorted(0).y) +
          sorted(0).x.toDouble * (sorted(2).y - sorted(1).y - i) / (sorted(
            2
          ).y - sorted(0).y),
        sorted(2).x.toDouble * i / (sorted(2).y - sorted(1).y) +
          sorted(1).x.toDouble * (sorted(2).y - sorted(1).y - i) / (sorted(
            2
          ).y - sorted(1).y)
      )
    )
    val lines = linesTop ++ linesBot
    lines.foldLeft(this)((im, yxX) =>
      im.line(Loc(yxX._2.toInt, yxX._1), Loc(yxX._3.toInt, yxX._1), c)
    )

  def circle(cen: Loc, r: Double, c: T): Image[T] =
    (0 to math.round(r / math.sqrt(2.0)).toInt).foldLeft(this)((im, i) =>
      val j = math.round(math.sqrt(r * r - i * i)).toInt
      im.updated(Loc(cen.x + j, cen.y + i), c)
        .updated(Loc(cen.x + j, cen.y - i), c)
        .updated(Loc(cen.x - j, cen.y + i), c)
        .updated(Loc(cen.x - j, cen.y - i), c)
        .updated(Loc(cen.x + i, cen.y + j), c)
        .updated(Loc(cen.x + i, cen.y - j), c)
        .updated(Loc(cen.x - i, cen.y + j), c)
        .updated(Loc(cen.x - i, cen.y - j), c)
    )

  def circleFill(cen: Loc, r: Double, c: T): Image[T] =
    (0 to math.round(r / math.sqrt(2.0)).toInt).foldLeft(this)((im, i) =>
      val j = math.round(math.sqrt(r * r - i * i)).toInt
      im.line(Loc(cen.x - j, cen.y + i), Loc(cen.x + j, cen.y + i), c)
        .line(Loc(cen.x - j, cen.y - i), Loc(cen.x + j, cen.y - i), c)
        .line(Loc(cen.x - i, cen.y + j), Loc(cen.x + i, cen.y + j), c)
        .line(Loc(cen.x - i, cen.y - j), Loc(cen.x + i, cen.y - j), c)
    )

  // assumes coords in cyclic order
  def quad(l0: Loc, l1: Loc, l2: Loc, l3: Loc, c: T): Image[T] =
    this.tri(l0, l1, l2, c).tri(l0, l2, l3, c)

  def lineThick(l0: Loc, l1: Loc, th: Double, c: T): Image[T] =
    val gr0 = (l1.y - l0.y).toDouble / (l1.x - l0.x)
    val gr = -1.0 / gr0
    val ang = math.atan(gr)
    val xd = math.round(th * math.cos(ang) / 2.0).toInt
    val yd = math.round(th * math.sin(ang) / 2.0).toInt
    this.quad(
      Loc(l0.x + xd, l0.y + yd),
      Loc(l1.x + xd, l1.y + yd),
      Loc(l1.x - xd, l1.y - yd),
      Loc(l0.x - xd, l0.y - yd),
      c
    )

case object Image:

  def blank[T](w: Int, h: Int, c: T): Image[T] =
    Image(w, h, Vector.fill[T](w * h)(c))

  def blank(w: Int, h: Int): Image[Colour] =
    blank(w, h, Colour(255, 255, 255))

  def fill[T](w: Int, h: Int)(f: Loc => T): Image[T] =
    val locs = for
      x <- 0 until w
      y <- 0 until h
    yield Loc(x, y)
    val im0 = Image.blank(w, h, f(Loc(0, 0)))
    locs.foldLeft(im0)((im, loc) => im.updated(loc, f(loc)))

  def toBI(im: Image[Colour]): BufferedImage =
    val canvas = new BufferedImage(im.w, im.h, BufferedImage.TYPE_INT_RGB)
    val wr = canvas.getRaster
    for (x <- 0 until im.w)
      for (y <- 0 until im.h)
        wr.setSample(x, y, 0, im(Loc(x, y)).r)
        wr.setSample(x, y, 1, im(Loc(x, y)).g)
        wr.setSample(x, y, 2, im(Loc(x, y)).b)
    canvas

  def saveAsPPM(im: Image[Colour], fileName: String): Unit =
    val fs = new java.io.FileWriter(fileName)
    fs.write(s"P3 ${im.w} ${im.h} 255\n")
    (0 until im.h).foreach(y =>
      (0 until im.w).foreach(x =>
        val p = im(Loc(x, y))
        fs.write(s"${p.r} ${p.g} ${p.b}\n")
      )
    )
    fs.close()

object CanvasApp:

  val white = Colour(255, 255, 255)
  val black = Colour(0, 0, 0)
  val red = Colour(255, 0, 0)
  val green = Colour(0, 255, 0)
  val darkGreen = Colour(0, 150, 0)
  val blue = Colour(0, 0, 255)

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
  ): Image[Int] =
    val res = iRange.toDouble / h
    Image.fill(w, h)(l =>
      val c = tl + Complex(l.x * res, -l.y * res)
      level(c, maxIts)
    )

  @main def main() =
    println("Hello")
    val maxIts = 60
    val im0 = mand(1000, 800, Complex(-2.5, 1.5), 3.0, maxIts)
    val im1 = im0 map (l =>
      l match
        case -1 => Colour(0, 0, 20) // very dark blue
        case li => Colour(255 * li / maxIts, 0, 0)
    )
    val bi = Image.toBI(im1)
    javax.imageio.ImageIO.write(bi, "png", new java.io.File("test8.png"))
    println("Goodbye")

// eof
