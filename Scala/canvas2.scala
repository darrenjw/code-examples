//> using scala 3.3.0

/*
canvas1.scala

Initial image/canvas app with minimal features
Creates a simple PPM

scala-cli canvas1.scala

*/


case class Colour(r: Int, g: Int, b: Int)

case class Image[T](w: Int, h: Int, data: Vector[T]):
  def apply(x: Int, y: Int): T = data(x*h+y)
  def map[S](f: T => S): Image[S] = Image(w, h, data map f)
  def updated(x: Int, y: Int, value: T): Image[T] =
    if ((x >= 0)&(y >= 0)&(x < w)&(y < h))
      Image(w, h, data.updated(x*h+y, value))
    else this
  def line(x0: Int, y0: Int, x1: Int, y1: Int, c: T): Image[T] =
    val xd = math.abs(x1 - x0)
    val yd = math.abs(y1 - y0)
    val n = math.max(xd, yd) + 1
    val is = (0 to n).toList
    val x = is.map(i => math.round(x0.toDouble*(n-i)/n + x1.toDouble*i/n).toInt)
    val y = is.map(i => math.round(y0.toDouble*(n-i)/n + y1.toDouble*i/n).toInt)
    val xy = x zip y
    xy.foldLeft(this)((im, xyi) => im.updated(xyi._1, xyi._2, c))

case object Image:
  def blank(w: Int, h: Int): Image[Colour] =
    Image(w, h, Vector.fill[Colour](w*h)(Colour(255, 255, 255)))
  def saveAsPPM(im: Image[Colour], fileName: String): Unit =
    val fs = new java.io.FileWriter(fileName)
    fs.write(s"P3 ${im.w} ${im.h} 255\n")
    (0 until im.h).foreach(y =>
      (0 until im.w).foreach(x =>
        val p = im(x, y)
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
      line(0, 0, 100, 50, black).
      line(0, 0, 50, 100, red).
      line(149, 50, 50 , 50, green)
    Image.saveAsPPM(im1, "test2.ppm")
    println("Goodbye")

// eof

