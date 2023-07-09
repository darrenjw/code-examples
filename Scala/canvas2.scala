//> using scala 3.3.0

/*
canvas2.scala

Initial image/canvas app with lines and filled triangles
Creates a simple PPM

scala-cli canvas1.scala

*/


case class Colour(r: Int, g: Int, b: Int)

case class Loc(x: Int, y: Int)

case class Image[T](w: Int, h: Int, data: Vector[T]):
  def apply(l: Loc): T = data(l.x*h+l.y)
  def map[S](f: T => S): Image[S] = Image(w, h, data map f)
  def updated(l: Loc, value: T): Image[T] =
    if ((l.x >= 0)&(l.y >= 0)&(l.x < w)&(l.y < h))
      Image(w, h, data.updated(l.x*h+l.y, value))
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
  val blue = Colour(0, 0, 255)

  @main def main() =
    println("Hello")
    val im = Image.blank(150, 100)
    val im1 = im.
      line(Loc(0, 0), Loc(100, 50), black).
      line(Loc(0, 0), Loc(50, 100), red).
      line(Loc(149, 50), Loc(50 , 50), green).
      updated(Loc(40, 40), blue)
    Image.saveAsPPM(im1, "test2.ppm")
    println("Goodbye")

// eof

