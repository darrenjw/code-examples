/*
logic.scala

Main logic of the agglomeration process simulation

*/

import java.awt.image.WritableRaster

object Logic:

  import Global.*

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
    wr.setSample(x, y, 0,  0) // dark green pixel
    wr.setSample(x, y, 1, 40) // dark green pixel
    wr.setSample(x, y, 2,  0) // dark green pixel

  def isAdjacent(wr: WritableRaster, x: Int, y: Int): Boolean =
    isOccupied(wr, x, y-1)|
    isOccupied(wr, x-1, y)|
    isOccupied(wr, x+1, y)|
    isOccupied(wr, x, y+1)

  def isOccupied(wr: WritableRaster, x: Int, y: Int): Boolean =
    (x >= 0)&&(y >= 0)&&(x < w)&&(y < h)&&(wr.getSample(x, y, 0) == 0)



// eof

