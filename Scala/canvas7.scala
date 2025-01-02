//> using scala 3.3.0

/*
canvas7.scala

Simple image/canvas app integrating the Lorenz system
https://en.wikipedia.org/wiki/Lorenz_system
Creates a simple PPM

scala-cli canvas7.scala

 */

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

  case class Pos(x: Double, y: Double, z: Double)

  def stepLorenz(rho: Double, sigma: Double, beta: Double, dt: Double)(
      p: Pos
  ): Pos =
    Pos(
      p.x + sigma * (p.y - p.x) * dt,
      p.y + (p.x * (rho - p.z) - p.y) * dt,
      p.z + (p.x * p.y - beta * p.z) * dt
    )

  @main def main() =
    println("Hello")
    val dt = 0.0001
    val termt = 100.0
    val step = stepLorenz(28.0, 10.0, 8.0 / 3.0, dt)
    val lorenz =
      LazyList.iterate(Pos(1.0, 0.5, 0.1))(step).take((termt / dt).toInt)
    val im0 = lorenz.foldLeft(Image.blank(1000, 800))((im, p) =>
      im.updated(Loc((500 + 20 * p.x).toInt, (400 + 14 * p.y).toInt), darkGreen)
    )
    Image.saveAsPPM(im0, "test7.ppm")
    println("Goodbye")

// eof
