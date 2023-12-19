import scala.util.Using

object Main extends App {
  private def calculateMinLoss(map: Array[Array[Int]], cache: Array[Array[Int]], x: Int, y: Int, numLines: Int, numCols: Int): Int = {
    val OUTSIDE_VALUE = 99999

    def minLoss(map: Array[Array[Int]], cache: Array[Array[Int]], x: Int, y: Int): Int = {
      if (x == 0 && y == 0) {
        return 0
      }

      if (cache(x)(y) != -1) cache(x)(y)

      val up1 = if (x - 1 >= 0) minLoss(map, cache, x - 1, y) + map(x)(y) else OUTSIDE_VALUE
      val up2 = if (x - 2 >= 0) minLoss(map, cache, x - 2, y) + map(x)(y) + map(x - 1)(y) else OUTSIDE_VALUE
      val up3 = if (x - 3 >= 0) minLoss(map, cache, x - 3, y) + map(x)(y) + map(x - 1)(y) + map(x - 2)(y) else OUTSIDE_VALUE

      val left1 = if (y - 1 >= 0) minLoss(map, cache, x, y - 1) + map(x)(y) else OUTSIDE_VALUE
      val left2 = if (y - 2 >= 0) minLoss(map, cache, x, y - 2) + map(x)(y) + map(x)(y - 1) else OUTSIDE_VALUE
      val left3 = if (y - 3 >= 0) minLoss(map, cache, x, y - 3) + map(x)(y) + map(x)(y - 1) + map(x)(y - 2) else OUTSIDE_VALUE

//      val right1 = if (y + 1 < numCols) minLoss(map, x, y + 1) + map(x)(y) else OUTSIDE_VALUE
//      val right2 = if (y + 2 < numCols) minLoss(map, x, y + 2) + map(x)(y) else OUTSIDE_VALUE
//      val right3 = if (y + 3 < numCols) minLoss(map, x, y + 3) + map(x)(y) else OUTSIDE_VALUE
//
//      val down1 = if (x + 1 < numLines) minLoss(map, x + 1, y) + map(x)(y) else OUTSIDE_VALUE
//      val down2 = if (x + 2 < numLines) minLoss(map, x + 2, y) + map(x)(y) else OUTSIDE_VALUE
//      val down3 = if (x + 3 < numLines) minLoss(map, x + 3, y) + map(x)(y) else OUTSIDE_VALUE

      val res = left1.min(left2).min(left3).min(up1).min(up2).min(up3)
      cache(x)(y) = res
      res
    }


    val res = minLoss(map, cache, x, y)
    println(res)
    res
  }

  val task1 = () => {
    Using(scala.io.Source.fromFile("dummy_input")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      val cityMap: Array[Array[Int]] = Array.ofDim(numLines, numCols)
      val cache: Array[Array[Int]] = Array.ofDim(numLines, numCols)

      lines.zipWithIndex.foreach { case(line, x) =>
        line.zipWithIndex.foreach { case(location, y) =>
          cityMap(x)(y) = location.toInt
          cache(x)(y) = -1
        }
      }

      println(calculateMinLoss(cityMap, cache, numLines - 1, numCols - 1, numLines, numCols))
    }
  }

  task1()
//  task2()
}
