import scala.util.Using

object Main extends App {
  private val MAX_MIRROR_REPEAT = 50

  // (0, 1) right
  // (0, -1) left
  // (1, 0) down
  // (-1, 0) up
  val getNeighbours = (currNode: (Int, Int, Int, Int), map: Array[Array[(Char, Int)]]) => {
    val numLines = map.length
    val numCols = map.head.length

    val neighs = scala.collection.mutable.ListBuffer[(Int, Int, Int, Int)]()
    val currX = currNode._1
    val currY = currNode._2
    val currXDir = currNode._3
    val currYDir = currNode._4

    map(currX)(currY)._1 match {
      case '.' => neighs.addOne(currX + currXDir, currY + currYDir, currXDir, currYDir)
      case '|' =>
        (currXDir, currYDir) match {
          case (0, 1) | (0, -1) =>
            neighs.addOne(currX - 1, currY, -1, 0)
            neighs.addOne(currX + 1, currY, 1, 0)
          case (1, 0) => neighs.addOne(currX + 1, currY, 1, 0)
          case (-1, 0) => neighs.addOne(currX - 1, currY, -1, 0)
        }
      case '-' =>
        (currXDir, currYDir) match {
          case (1, 0) | (-1, 0) =>
            neighs.addOne(currX, currY + 1, 0, 1)
            neighs.addOne(currX, currY - 1, 0, -1)
          case (0, 1) => neighs.addOne(currX, currY + 1, 0, 1)
          case (0, -1) => neighs.addOne(currX, currY - 1, 0, -1)
        }
      case '/' =>
        (currXDir, currYDir) match {
          case (1, 0) => neighs.addOne(currX, currY - 1, 0, -1)
          case (0, 1) => neighs.addOne(currX - 1, currY, -1, 0)
          case (-1, 0) => neighs.addOne(currX, currY + 1, 0, 1)
          case (0, -1) => neighs.addOne(currX + 1, currY, 1, 0)
        }
      case '\\' =>
        (currXDir, currYDir) match {
          case (1, 0) => neighs.addOne(currX, currY + 1, 0, 1)
          case (0, 1) => neighs.addOne(currX + 1, currY, 1, 0)
          case (-1, 0) => neighs.addOne(currX, currY - 1, 0, -1)
          case (0, -1) => neighs.addOne(currX - 1, currY, -1, 0)
        }
    }

    neighs.filter(neigh => {
      val neighX = neigh._1
      val neighY = neigh._2
      neighX >= 0 && neighX < numLines && neighY >= 0 && neighY < numCols && map(neighX)(neighY)._2 < MAX_MIRROR_REPEAT
    })
  }

  val calculateEnergized = (initialNode: (Int, Int, Int, Int), map: Array[Array[(Char, Int)]]) => {
    val queue = scala.collection.mutable.Queue[(Int, Int, Int, Int)](initialNode)
    val initialX = initialNode._1
    val initialY = initialNode._2
    map(initialX)(initialY) = map(initialX)(initialY).copy(_2 = 1)
    var energizedBoxes = 1
    while (queue.nonEmpty) {
      val currNode = queue.dequeue()
      for (neigh <- getNeighbours(currNode, map)) {
        val neighX = neigh._1
        val neighY = neigh._2
        if (map(neighX)(neighY)._2 == 0) {
          energizedBoxes += 1
        }
        map(neighX)(neighY) = map(neighX)(neighY).copy(_2 = map(neighX)(neighY)._2 + 1)
        queue += neigh
      }
    }
    energizedBoxes
  }

  val resetMap = (map: Array[Array[(Char, Int)]]) => {
    val numLines = map.length
    val numCols = map.head.length

    for (x <- 0 until numLines) {
      for (y <- 0 until numCols) {
        map(x)(y) = (map(x)(y)._1, 0)
      }
    }
  }

  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      val mirrorMap: Array[Array[(Char, Int)]] = Array.ofDim(numLines, numCols)

      lines.zipWithIndex.foreach { case(line, x) =>
        line.zipWithIndex.foreach { case(mirror, y) =>
          mirrorMap(x)(y) = (mirror, 0)
        }
      }

      println(calculateEnergized((0, 0, 0, 1), mirrorMap))
    }
  }

  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      val mirrorMap: Array[Array[(Char, Int)]] = Array.ofDim(numLines, numCols)

      lines.zipWithIndex.foreach { case (line, x) =>
        line.zipWithIndex.foreach { case (mirror, y) =>
          mirrorMap(x)(y) = (mirror, 0)
        }
      }

      var maxEnergized = 0
      for (x <- 0 until numLines) {
        maxEnergized = maxEnergized.max(calculateEnergized((x, 0, 0, 1), mirrorMap))
        resetMap(mirrorMap)

        maxEnergized = maxEnergized.max(calculateEnergized((x, numCols - 1, 0, -1), mirrorMap))
        resetMap(mirrorMap)
      }

      for (y <- 0 until numCols) {
        maxEnergized = maxEnergized.max(calculateEnergized((0, y, 1, 0), mirrorMap))
        resetMap(mirrorMap)

        maxEnergized = maxEnergized.max(calculateEnergized((numLines - 1, y, -1, 0), mirrorMap))
        resetMap(mirrorMap)
      }

      println(maxEnergized)
    }
  }

  task1()
  task2()
}
