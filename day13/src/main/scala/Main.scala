import scala.util.Using

object Main extends App {
  private def splitArray(arr: List[String]): List[List[String]] = {
    arr match {
      case Nil => List.empty
      case _ =>
        val (head, rest) = arr.span(str => str.strip() != "")
        List(head) ++ splitArray(rest.drop(1))
    }
  }

  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val res = splitArray(lines).map(group => {
        val numLines = group.length
        val numCols = group.head.length
        val patternMap: Array[Array[Char]] = Array.ofDim(numLines, numCols)
        group.zipWithIndex.foreach { case (line, x) =>
          line.zipWithIndex.foreach { case (location, y) =>
            patternMap(x)(y) = location
          }
        }

        var splitLocation = (0, 0)
        for (x <- 1 until numLines) {
          var isMirror = true
          var mirrorLength = 0
          var mirrorStep = 1
          while (x - mirrorStep >= 0 && x + mirrorStep - 1 < numLines && isMirror) {
            val topLine = x - mirrorStep
            val bottomLine = x + mirrorStep - 1
            for (y <- 0 until numCols) {
              if (patternMap(topLine)(y) != patternMap(bottomLine)(y)) {
                isMirror = false
              }
            }

            if (isMirror) {
              mirrorLength += 1
            }
            mirrorStep += 1
          }

          if (isMirror) {
            splitLocation = (0, x)
          }
        }

        for (y <- 1 until numCols) {
          var isMirror = true
          var mirrorLength = 0
          var mirrorStep = 1
          while (y - mirrorStep >= 0 && y + mirrorStep - 1 < numCols && isMirror) {
            val leftCol = y - mirrorStep
            val rightCol = y + mirrorStep - 1
            for (x <- 0 until numLines) {
              if (patternMap(x)(leftCol) != patternMap(x)(rightCol)) {
                isMirror = false
              }
            }

            if (isMirror) {
              mirrorLength += 1
            }
            mirrorStep += 1
          }

          if (isMirror) {
            splitLocation = (1, y)
          }
        }

        splitLocation match {
          case (1, col) => col
          case (0, line) => 100 * line
        }
      }).sum

      println(res)
    }
  }

  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val res = splitArray(lines).map(group => {
        val numLines = group.length
        val numCols = group.head.length
        val patternMap: Array[Array[Char]] = Array.ofDim(numLines, numCols)
        group.zipWithIndex.foreach { case (line, x) =>
          line.zipWithIndex.foreach { case (location, y) =>
            patternMap(x)(y) = location
          }
        }

        var splitLocation = (0, 0)
        for (x <- 1 until numLines) {
          var isMirror = true
          var mirrorLength = 0
          var mirrorStep = 1
          var smudgeFound = 0
          while (x - mirrorStep >= 0 && x + mirrorStep - 1 < numLines && isMirror && smudgeFound <= 1) {
            val topLine = x - mirrorStep
            val bottomLine = x + mirrorStep - 1
            for (y <- 0 until numCols) {
              if (patternMap(topLine)(y) != patternMap(bottomLine)(y)) {
                smudgeFound += 1
              }

              if (smudgeFound > 1) {
                isMirror = false
              }
            }

            if (isMirror) {
              mirrorLength += 1
            }
            mirrorStep += 1
          }

          if (isMirror && smudgeFound == 1) {
            splitLocation = (0, x)
          }
        }

        for (y <- 1 until numCols) {
          var isMirror = true
          var mirrorLength = 0
          var mirrorStep = 1
          var smudgeFound = 0
          while (y - mirrorStep >= 0 && y + mirrorStep - 1 < numCols && isMirror && smudgeFound <= 1) {
            val leftCol = y - mirrorStep
            val rightCol = y + mirrorStep - 1
            for (x <- 0 until numLines) {
              if (patternMap(x)(leftCol) != patternMap(x)(rightCol)) {
                smudgeFound += 1
              }

              if (smudgeFound > 1) {
                isMirror = false
              }
            }

            if (isMirror) {
              mirrorLength += 1
            }
            mirrorStep += 1
          }

          if (isMirror && smudgeFound == 1) {
            splitLocation = (1, y)
          }
        }

        splitLocation match {
          case (1, col) => col
          case (0, line) => 100 * line
        }
      }).sum

      println(res)
    }
  }

  task1()
  task2()
}
