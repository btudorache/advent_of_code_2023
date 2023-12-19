import scala.util.Using

object Main extends App {
  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      var currX = 0
      var currY = 0
      val borderSet = scala.collection.mutable.Set[(Int, Int)]((currX, currY))
      var area = 1
      lines.foreach(line => {
        val args = line.split(" ")
        val direction = args(0)
        val size = args(1).toInt
        area += size
        direction match {
          case "R" =>
            for (y <- 1 to size) {
              borderSet.add((currX, currY + y))
            }
            currY += size
          case "L" =>
            for (y <- 1 to size) {
              borderSet.add((currX, currY - y))
            }
            currY -= size
          case "U" =>
            for (x <- 1 to size) {
              borderSet.add((currX - x, currY))
            }
            currX -= size
          case "D" =>
            for (x <- 1 to size) {
              borderSet.add((currX + x, currY))
            }
            currX += size
        }
      })

      val START_NODE = (1, 1)
      val visited = scala.collection.mutable.Set[(Int, Int)](START_NODE)
      val queue = scala.collection.mutable.Queue(START_NODE)
      val directions = List((0, 1), (1, 0), (-1, 0), (0, -1))
      while (queue.nonEmpty) {
        val currNode = queue.dequeue()
        val currX = currNode._1
        val currY = currNode._2
        for (direction <- directions) {
          val neighX = currX + direction._1
          val neighY = currY + direction._2
          if (!visited.contains((neighX, neighY)) && !borderSet.contains((neighX, neighY))) {
            area += 1
            visited.add(neighX, neighY)
            queue.addOne((neighX, neighY))
          }
        }
      }

      println(area)
    }
  }

  task1()

  // apparently part 2 needs shoelace formula (the same as the one for day 10, algorithm for calculating area of polygon when the points are known)
  //  task2()
}
