import scala.util.Using

object Main extends App {
  val isValidPosition = (map: Array[Array[(Char, Boolean, Int)]], x: Int, y: Int, maxLines: Int, maxCols: Int) => {
    if (x >= 0 && x < maxLines && y >= 0 && y < maxCols && !map(x)(y)._2) true else false
  }

  val getValidNeighbours = (map: Array[Array[(Char, Boolean, Int)]], x: Int, y: Int, maxLines: Int, maxCols: Int) => {
    var locations: Array[(Int, Int)] = Array.empty
    val locationType = map(x)(y)._1
    locationType match {
      case '|' => locations = Array((x + 1, y), (x - 1, y))
      case '-' => locations = Array((x, y - 1), (x, y + 1))
      case 'L' => locations = Array((x - 1, y), (x, y + 1))
      case 'J' => locations = Array((x - 1, y), (x, y - 1))
      case '7' => locations = Array((x + 1, y), (x, y - 1))
      case 'F' => locations = Array((x + 1, y), (x, y + 1))
      case 'S' =>
        if (isValidPosition(map, x - 1, y, maxLines, maxCols) && List('F', '7', '|').contains(map(x - 1)(y)._1)) {
          locations = locations :+ ((x - 1, y))
        }
        if (isValidPosition(map, x + 1, y, maxLines, maxCols) && List('L', 'J', '|').contains(map(x + 1)(y)._1)) {
          locations = locations :+ ((x + 1, y))
        }
        if (isValidPosition(map, x, y - 1, maxLines, maxCols) && List('L', 'F', '-').contains(map(x)(y - 1)._1)) {
          locations = locations :+ ((x, y - 1))
        }
        if (isValidPosition(map, x, y + 1, maxLines, maxCols) && List('7', 'J', '-').contains(map(x)(y + 1)._1)) {
          locations = locations :+ ((x, y + 1))
        }
      case _ => locations = Array()
    }

    locations.filter(location => isValidPosition(map, location._1, location._2, maxLines, maxCols))
  }

  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      val monsterMap: Array[Array[(Char, Boolean, Int)]] = Array.ofDim[(Char, Boolean, Int)](numLines, numCols)

      var startPoint = (0, 0)
      lines.zipWithIndex.foreach { case (line, x) =>
        line.zipWithIndex.foreach { case (location, y) =>
          monsterMap(x)(y) = (location, false, 0)
          if (location == 'S') {
            startPoint = (x, y)
          }
        }
      }

      var maxDistance = 0
      val queue = scala.collection.mutable.Queue(startPoint)
      monsterMap(startPoint._1)(startPoint._2) = monsterMap(startPoint._1)(startPoint._2).copy(_2 = true)
      while (queue.nonEmpty) {
        val currNode = queue.dequeue()
        val currX = currNode._1
        val currY = currNode._2
        for (node <- getValidNeighbours(monsterMap, currX, currY, numLines, numCols)) {
          val nodeX = node._1
          val nodeY = node._2
          monsterMap(nodeX)(nodeY) = monsterMap(nodeX)(nodeY).copy(_2 = true, _3 = monsterMap(currX)(currY)._3 + 1)
          maxDistance = maxDistance.max(monsterMap(nodeX)(nodeY)._3)
          queue.append((nodeX, nodeY))
        }
      }

      println(maxDistance)
    }
  }

  val getLocationType = (location: Char) => {
    location match {
      case '.' => 0
      case '-' => 1
      case '|' | 'L' | 'J' | '7' | 'F' => 2
    }
  }

  // too hard; use some sort of flood fill around the main loop or lookup Jordan Curve Theorem
  val task2 = () => {
    Using(scala.io.Source.fromFile("dummy_input2")) { source =>
      val lines = source.getLines().toList
      val numLines = lines.length
      val numCols = lines.head.length
      // type 0 -> ground
      // type 1 -> horizontal
      // type 2 -> vertical
      val monsterMap: Array[Array[(Char, Boolean, Int)]] = Array.ofDim[(Char, Boolean, Int)](numLines, numCols)

      var startPoint = (0, 0)
      lines.zipWithIndex.foreach { case (line, x) =>
        line.zipWithIndex.foreach { case (location, y) =>
          if (location == 'S') {
            startPoint = (x, y)
            monsterMap(x)(y) = (location, false, -1)
          } else {
            monsterMap(x)(y) = (location, false, getLocationType(location))
          }
        }
      }

      if ((startPoint._1 - 1 >= 0 && monsterMap(startPoint._1 - 1)(startPoint._2)._3 == 2) || (startPoint._1 + 1 < numLines && monsterMap(startPoint._1 + 1)(startPoint._2)._3 == 2)) {
        monsterMap(startPoint._1)(startPoint._2) = monsterMap(startPoint._1)(startPoint._2).copy(_3 = 2)
      } else {
        monsterMap(startPoint._1)(startPoint._2) = monsterMap(startPoint._1)(startPoint._2).copy(_3 = 1)
      }

      val queue = scala.collection.mutable.Queue(startPoint)
      monsterMap(startPoint._1)(startPoint._2) = monsterMap(startPoint._1)(startPoint._2).copy(_2 = true)
      while (queue.nonEmpty) {
        val currNode = queue.dequeue()
        val currX = currNode._1
        val currY = currNode._2
        for (node <- getValidNeighbours(monsterMap, currX, currY, numLines, numCols)) {
          val nodeX = node._1
          val nodeY = node._2
          monsterMap(nodeX)(nodeY) = monsterMap(nodeX)(nodeY).copy(_2 = true)
          queue.append((nodeX, nodeY))
        }
      }

      var numEnclosed = 0
      for (y <- 0 until numCols) {
        var isInside = if (monsterMap(0)(y)._3 == 0) false else true
        for (x <- 0 until numLines) {
          if ((monsterMap(x)(y)._3 == 0 || !monsterMap(x)(y)._2) && isInside) {
            numEnclosed += 1
          } else if (monsterMap(x)(y)._3 == 1) {
            isInside = !isInside
          }

        }
      }

      println(numEnclosed)
    }
  }

  task1()
  task2()
}
