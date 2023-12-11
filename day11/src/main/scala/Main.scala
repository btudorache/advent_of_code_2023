import scala.math.abs
import scala.util.Using

object Main extends App {
  val distance = (point1: (Int, Int), point2: (Int, Int)) => {
    abs(point1._1 - point2._1) + abs(point1._2 - point2._2)
  }

  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      val initialMap: Array[Array[Char]] = Array.ofDim[Char](numLines, numCols)

      val repeatingLines = scala.collection.mutable.ArrayBuffer[Int]()
      val repeatingCols = scala.collection.mutable.ArrayBuffer[Int]()
      lines.zipWithIndex.foreach { case (line, x) =>
        var lineEmpty = true
        line.zipWithIndex.foreach { case (location, y) =>
          initialMap(x)(y) = location
          if (location == '#') {
            lineEmpty = false
          }
        }

        if (lineEmpty) {
          repeatingLines += x
        }
      }

      for (y <- 0 until numCols) {
        var colEmpty = true
        for (x <- 0 until numLines) {
           if (initialMap(x)(y) == '#') {
             colEmpty = false
           }
        }

        if (colEmpty) {
          repeatingCols += y
        }
      }

      val expandedLinesMap: Array[Array[Char]] = Array.ofDim[Char](numLines + repeatingLines.length, numCols)
      var currX = 0
      for (x <- 0 until numLines) {
        expandedLinesMap(currX) = Array(initialMap(x): _*)
        if (repeatingLines.contains(x)) {
          currX += 1
          expandedLinesMap(currX) = Array(initialMap(x): _*)
        }

        currX += 1
      }

      val finalMap: Array[Array[Char]] = Array.ofDim[Char](numLines + repeatingLines.length, numCols + repeatingCols.length)
      val galaxies = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
      var currY = 0
      for (x <- 0 until  numLines + repeatingLines.length) {
        currY = 0
        for (y <- 0 until numCols) {
          finalMap(x)(currY) = expandedLinesMap(x)(y)
          if (repeatingCols.contains(y)) {
            currY += 1
            finalMap(x)(currY) = expandedLinesMap(x)(y)
          }

          if (finalMap(x)(currY) == '#') {
            galaxies.append((x, currY))
          }
          currY += 1
        }
      }

      var distances = 0
      for (point1 <- galaxies.indices) {
        for (point2 <- point1 + 1 until galaxies.length) {
          distances += distance(galaxies(point1), galaxies(point2))
        }
      }

      println(distances)
    }
  }

  val distance2 = (point1: (Int, Int), point2: (Int, Int), sectionMap: scala.collection.mutable.Map[(Int, Int), (Int, Int)], sectionMultiplier: Int) => {
    val section1 = sectionMap(point1)
    val section2 = sectionMap(point2)
    val sectionDistance = abs(section1._1 - section2._1) + abs(section1._2 - section2._2)

    abs(point1._1 - point2._1) + abs(point1._2 - point2._2) + sectionDistance * sectionMultiplier
  }

  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      val initialMap: Array[Array[Char]] = Array.ofDim[Char](numLines, numCols)

      val repeatingLines = scala.collection.mutable.ArrayBuffer[Int]()
      val repeatingCols = scala.collection.mutable.ArrayBuffer[Int]()
      lines.zipWithIndex.foreach { case (line, x) =>
        var lineEmpty = true
        line.zipWithIndex.foreach { case (location, y) =>
          initialMap(x)(y) = location
          if (location == '#') {
            lineEmpty = false
          }
        }

        if (lineEmpty) {
          repeatingLines += x
        }
      }

      for (y <- 0 until numCols) {
        var colEmpty = true
        for (x <- 0 until numLines) {
          if (initialMap(x)(y) == '#') {
            colEmpty = false
          }
        }

        if (colEmpty) {
          repeatingCols += y
        }
      }

      var xSection = 0
      val galaxiesSections = scala.collection.mutable.Map[(Int, Int), (Int, Int)]()
      for (x <- 0 until numLines) {
        if (repeatingLines.contains(x)) {
          xSection += 1
        }

        var ySection = 0
        for (y <- 0 until numCols) {
          if (repeatingCols.contains(y)) {
            ySection += 1
          }

          if (initialMap(x)(y) == '#') {
            galaxiesSections.addOne((x, y), (xSection, ySection))
          }
        }
      }

      var distances: BigInt = 0
      val galaxies = galaxiesSections.keys.toList
      for (point1 <- galaxies.indices) {
        for (point2 <- point1 + 1 until galaxies.length) {
          distances += distance2(galaxies(point1), galaxies(point2), galaxiesSections, 1000000 - 1)
        }
      }

      println(distances)
    }
  }

  task1()
  task2()
}
