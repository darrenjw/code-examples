//> using scala 3.3.0

/*
canvas5.scala

Simple image/canvas app drawing a fractal fern
Creates a simple PPM

scala-cli canvas5.scala

*/


case class Colour(r: Int, g: Int, b: Int)

case class Loc(x: Int, y: Int):

  def mid(l: Loc) = Loc((x+l.x)/2, (y+l.y)/2)

case class Image[T](w: Int, h: Int, data: Vector[T]):

  def apply(l: Loc): T = data(l.x*h + l.y)

  def map[S](f: T => S): Image[S] = Image(w, h, data map f)

  def updated(l: Loc, value: T): Image[T] =
    if ((l.x >= 0)&(l.y >= 0)&(l.x < w)&(l.y < h))
      Image(w, h, data.updated(l.x*h + l.y, value))
    else this

  def line(l0: Loc, l1: Loc, c: T): Image[T] =
    val xd = math.abs(l1.x - l0.x)
    val yd = math.abs(l1.y - l0.y)
    val n = math.max(xd, yd) + 1
    val is = (0 to n).toList
    val x = is.map(i => math.round(l0.x.toDouble*(n-i)/n + l1.x.toDouble*i/n).toInt)
    val y = is.map(i => math.round(l0.y.toDouble*(n-i)/n + l1.y.toDouble*i/n).toInt)
    val ls = (x zip y) map (p => Loc(p._1, p._2))
    ls.foldLeft(this)((im, li) => im.updated(li, c))

  def tri(l0: Loc, l1: Loc, l2: Loc, c: T): Image[T] =
    val sorted = List(l0, l1, l2).sortWith(_.y < _.y)
    val iTop = 0 until (sorted(1).y - sorted(0).y)
    val linesTop = iTop.map(i => (sorted(0).y + i,
      sorted(2).x.toDouble*i/(sorted(2).y-sorted(0).y) +
        sorted(0).x.toDouble*(sorted(2).y-sorted(0).y-i)/(sorted(2).y-sorted(0).y),
      sorted(1).x.toDouble*i/(sorted(1).y-sorted(0).y) +
        sorted(0).x.toDouble*(sorted(1).y-sorted(0).y-i)/(sorted(1).y-sorted(0).y)))
    val iBot = 0 until (sorted(2).y - sorted(1).y)
    val linesBot = iBot.map(i => (sorted(1).y + i,
      sorted(2).x.toDouble*(sorted(1).y - sorted(0).y + i)/(sorted(2).y-sorted(0).y) +
        sorted(0).x.toDouble*(sorted(2).y-sorted(1).y-i)/(sorted(2).y-sorted(0).y),
      sorted(2).x.toDouble*i/(sorted(2).y-sorted(1).y) +
        sorted(1).x.toDouble*(sorted(2).y-sorted(1).y-i)/(sorted(2).y-sorted(1).y)))
    val lines = linesTop ++ linesBot
    lines.foldLeft(this)((im, yxX) =>
      im.line(Loc(yxX._2.toInt, yxX._1), Loc(yxX._3.toInt, yxX._1), c))

  def circle(cen: Loc, r: Double, c: T): Image[T] =
    (0 to math.round(r/math.sqrt(2.0)).toInt).foldLeft(this)((im, i) =>
      val j = math.round(math.sqrt(r*r - i*i)).toInt
      im.
        updated(Loc(cen.x + j, cen.y + i), c).
        updated(Loc(cen.x + j, cen.y - i), c).
        updated(Loc(cen.x - j, cen.y + i), c).
        updated(Loc(cen.x - j, cen.y - i), c).
        updated(Loc(cen.x + i, cen.y + j), c).
        updated(Loc(cen.x + i, cen.y - j), c).
        updated(Loc(cen.x - i, cen.y + j), c).
        updated(Loc(cen.x - i, cen.y - j), c)
      )

  def circleFill(cen: Loc, r: Double, c: T): Image[T] = 
    (0 to math.round(r/math.sqrt(2.0)).toInt).foldLeft(this)((im, i) =>
      val j = math.round(math.sqrt(r*r - i*i)).toInt
      im.
        line(Loc(cen.x - j, cen.y + i), Loc(cen.x + j, cen.y + i), c).
        line(Loc(cen.x - j, cen.y - i), Loc(cen.x + j, cen.y - i), c).
        line(Loc(cen.x - i, cen.y + j), Loc(cen.x + i, cen.y + j), c).
        line(Loc(cen.x - i, cen.y - j), Loc(cen.x + i, cen.y - j), c)
      )

  // assumes coords in cyclic order
  def quad(l0: Loc, l1: Loc, l2: Loc, l3: Loc, c: T): Image[T] =
    this.tri(l0, l1, l2, c).tri(l0, l2, l3, c)

  def lineThick(l0: Loc, l1: Loc, th: Double, c: T): Image[T] =
    val gr0 = (l1.y - l0.y).toDouble/(l1.x - l0.x)
    val gr = -1.0/gr0
    val ang = math.atan(gr)
    val xd = math.round(th*math.cos(ang)/2.0).toInt
    val yd = math.round(th*math.sin(ang)/2.0).toInt
    this.quad(Loc(l0.x+xd, l0.y+yd), Loc(l1.x+xd, l1.y+yd),
      Loc(l1.x-xd, l1.y-yd), Loc(l0.x-xd, l0.y-yd), c)


case object Image:

  def blank[T](w: Int, h: Int, c: T): Image[T] =
    Image(w, h, Vector.fill[T](w*h)(c))

  def blank(w: Int, h: Int): Image[Colour] =
    blank(w, h, Colour(255, 255, 255))

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

  extension [T] (im: Image[T])
    def fern(lev: Int, x0: Double, y0: Double, x1: Double, y1: Double, squ: Double, c: T): Image[T] =
      val tc = 0.05 // thickness coef
      val hs = 0.6 // horizontal shrink factor
      val sq = 0.7 // horizontal squish factor
      val vs = 0.9 // vertical shrink factor
      val rbf = 0.7 // right branch fraction
      val vr = 0.03 // vertical rotation angle (radians)
      val l = math.sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0))
      val th = tc*l
      val im0 = im.lineThick(Loc(math.round(x0).toInt, math.round(y0).toInt),
        Loc(math.round(x1).toInt, math.round(y1).toInt), th, c)
      if (lev == 0)
        im0
      else
        val xd = x1 - x0
        val yd = y1 - y0
        // left branch
        val lx2 = x1 + (1.0/math.sqrt(2))*xd*hs*squ + (1.0/math.sqrt(2))*yd*hs*squ
        val ly2 = y1 - (1.0/math.sqrt(2))*xd*hs*squ + (1.0/math.sqrt(2))*yd*hs*squ
        // right branch
        val rx2 = x0 + rbf*(x1-x0) + (1.0/math.sqrt(2))*xd*hs*squ - (1.0/math.sqrt(2))*yd*hs*squ
        val ry2 = y0 + rbf*(y1-y0) + (1.0/math.sqrt(2))*xd*hs*squ + (1.0/math.sqrt(2))*yd*hs*squ
        // top branch
        val tx2 = x1 + math.cos(vr)*xd*vs - math.sin(vr)*yd*vs
        val ty2 = y1 + math.sin(vr)*xd*vs + math.cos(vr)*yd*vs
        // add branches
        im0.
          fern(lev - 1, x1, y1, lx2, ly2, sq*squ, c). // left branch
          fern(lev - 1, x0 + rbf*(x1-x0), y0 + rbf*(y1-y0), rx2, ry2, sq*squ, c). // right branch
          fern(lev - 1, x1, y1, tx2, ty2, squ, c) // top branch
  
  @main def main() =
    println("Hello")
    val im0 = Image.blank(800, 900).
      fern(15, 400, 870, 400, 770, 0.7, darkGreen)

    Image.saveAsPPM(im0, "test5.ppm")
    println("Goodbye")

// eof

