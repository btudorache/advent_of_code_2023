import scala.util.Using

object Main extends App {
  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val times = lines.head.split(": ")(1).strip().split(" ").filter(str => str != "").map(str => str.toInt)
      val distances = lines(1).split(": ")(1).strip().split(" ").filter(str => str != "").map(str => str.toInt)

      val res = times.zip(distances).map(pair => {
        val totalDistance = pair._2
        val totalTime = pair._1
        (1 until pair._1).map(currTime => {
          currTime * (totalTime - currTime)
        }).count(distance => distance > totalDistance)
      }).product

      println(res)
    }
  }

  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val time = BigInt(lines.head.split(": ")(1).strip().split(" ").filter(str => str != "").mkString(""))
      val distance = BigInt(lines(1).split(": ")(1).strip().split(" ").filter(str => str != "").mkString(""))

      var minBound = BigInt(0)
      var maxBound = BigInt(0)

      var start = BigInt(0)
      var end = time
      while (start < end) {
        val mid = (start + end ) / 2
        val result = mid * (time - mid) - distance
        if (result > 0) {
          end = mid - 1
        } else if (result < 0) {
          start = mid + 1
        }
      }
      minBound = end

      start = BigInt(0)
      end = time
      while (start < end) {
        val mid = (start + end) / 2
        val result = mid * (time - mid) - distance
        if (result > 0) {
          start = mid + 1
        } else if (result < 0) {
          end = mid - 1
        }
      }
      maxBound = start
      println(maxBound - minBound)
    }
  }

  task1()
  task2()
}
