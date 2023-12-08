import scala.util.Using

object Main extends App {
  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val map = scala.collection.mutable.Map[String, (String, String)]()
      val path = lines.head
      lines.tail.drop(1).foreach(line => {
        val args = line.split(" = ")
        val values = args(1).split(", ")
        map(args(0)) = (values(0).substring(1), values(1).substring(0, values(1).length - 1))
      })

      var currLocation = "AAA"
      var steps = 0
      var index = 0
      while (currLocation != "ZZZ") {
        val direction = path(index)
        currLocation = if (direction == 'L') map(currLocation)._1 else map(currLocation)._2
        index += 1
        if (index == path.length) {
          index = 0
        }
        steps += 1
      }

      println(steps)
    }
  }

  private def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) {
    (a, b) =>
      b * a /
        LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val map = scala.collection.mutable.Map[String, (String, String)]()
      val path = lines.head
      lines.tail.drop(1).foreach(line => {
        val args = line.split(" = ")
        val values = args(1).split(", ")
        map(args(0)) = (values(0).substring(1), values(1).substring(0, values(1).length - 1))
      })

      val currLocations = map.keys.toList.filter(key => key.endsWith("A"))
      val steps = currLocations.map(location => {
        var currLocation = location
        var steps = 0
        var index = 0
         while (!currLocation.endsWith("Z")) {
           val direction = path(index)
           currLocation = if (direction == 'L') map(currLocation)._1 else map(currLocation)._2
           index += 1
           if (index == path.length) {
             index = 0
           }
           steps += 1
         }

        BigInt(steps)
      })

      println(lcm(steps))
    }
  }

  task1()
  task2()
}
