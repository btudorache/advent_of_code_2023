import scala.util.Using

object Main extends App {
  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      val platformMap: Array[Array[Char]] = Array.ofDim(numLines, numCols)

      lines.zipWithIndex.foreach { case (line, x) =>
        line.zipWithIndex.foreach { case (location, y) =>
          platformMap(x)(y) = location
        }
      }

      val invalidLocations = List('#', 'O')
      for (y <- 0 until numCols) {
        for (x <- 0 until numLines) {
          if (platformMap(x)(y) == 'O') {
            var upperPos = x
            while (upperPos - 1 >= 0 && !invalidLocations.contains(platformMap(upperPos - 1)(y))) {
              upperPos -= 1
            }
            platformMap(x)(y) = '.'
            platformMap(upperPos)(y) = 'O'
          }
        }
      }

      val res = (0 until numLines).map(line => platformMap(line).count(char => char == 'O') * (numLines - line)).sum
      println(res)
    }
  }

  // the cycles begin to repeat after some time (I can identify the cicle, but I can't identify exactly the rule for any input (I can identify it by hand though))
  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      val platformMap: Array[Array[Char]] = Array.ofDim(numLines, numCols)

      lines.zipWithIndex.foreach { case (line, x) =>
        line.zipWithIndex.foreach { case (location, y) =>
          platformMap(x)(y) = location
        }
      }

      val invalidLocations = List('#', 'O')

      for (i <- 1 to 120) {
        for (y <- 0 until numCols) {
          for (x <- 0 until numLines) {
            if (platformMap(x)(y) == 'O') {
              var upperPos = x
              while (upperPos - 1 >= 0 && !invalidLocations.contains(platformMap(upperPos - 1)(y))) {
                upperPos -= 1
              }
              platformMap(x)(y) = '.'
              platformMap(upperPos)(y) = 'O'
            }
          }
        }

        for (x <- 0 until numLines) {
          for (y <- 0 until numCols) {
            if (platformMap(x)(y) == 'O') {
              var leftPos = y
              while (leftPos - 1 >= 0 && !invalidLocations.contains(platformMap(x)(leftPos - 1))) {
                leftPos -= 1
              }
              platformMap(x)(y) = '.'
              platformMap(x)(leftPos) = 'O'
            }
          }
        }

        for (y <- 0 until numCols) {
          for (x <- (0 until numLines).reverse) {
            if (platformMap(x)(y) == 'O') {
              var lowerPos = x
              while (lowerPos + 1 < numLines && !invalidLocations.contains(platformMap(lowerPos + 1)(y))) {
                lowerPos += 1
              }
              platformMap(x)(y) = '.'
              platformMap(lowerPos)(y) = 'O'
            }
          }
        }

        for (x <- 0 until numLines) {
          for (y <- (0 until numCols).reverse) {
            if (platformMap(x)(y) == 'O') {
              var rightPos = y
              while (rightPos + 1 < numCols && !invalidLocations.contains(platformMap(x)(rightPos + 1))) {
                rightPos += 1
              }
              platformMap(x)(y) = '.'
              platformMap(x)(rightPos) = 'O'
            }
          }
        }

        val res = (0 until numLines).map(line => platformMap(line).count(char => char == 'O') * (numLines - line)).sum
        println(f"${res} --- cycle: ${i}")
      }

      // TODO: find cycle start
      val cicle_start = 105
      val total = 1000000000
      // TODO: find cycle len
      val cycle_len = 13

      val remainder = (total - cicle_start) % cycle_len
      println(remainder)
    }
  }

  task1()
  task2()
}
