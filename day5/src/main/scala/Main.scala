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
      val split = splitArray(lines)

      val seeds = split.head.head.split(": ")(1).split(" ").map(str => BigInt(str.strip()))

      val mappers = split.tail.map(mapperList => {
        mapperList.drop(1).map(line => {
          // ranges(0) dest start
          // ranges(1) src start
          // ranges(2) range length
         line.split(" ").filter(str => str.strip() != "").map(str => BigInt(str))
        })
      })

      val minLocation = seeds.map(seed => {
        mappers.foldLeft(seed)((acc, ranges) => {
          var dest = acc
          for (range <- ranges) {
            if (range(1) <= acc && acc <= range(1) + range(2)) {
              dest = range(0) + acc - range(1)
            }
          }
          dest
        })
      }).reduce((a, b) => a.min(b))

      println(minLocation)
    }
  }

  // For task2, I think it works with some sort of range mapping (but that's too hard for me)
  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val split = splitArray(lines)

      val seeds = split.head.head.split(": ")(1).split(" ").map(str => BigInt(str.strip()))

      val mappers = split.tail.map(mapperList => {
        mapperList.drop(1).map(line => {
          // ranges(0) dest start
          // ranges(1) src start
          // ranges(2) range length
          line.split(" ").filter(str => str.strip() != "").map(str => BigInt(str))
        })
      })

      val minLocation = seeds.map(seed => {
        mappers.foldLeft(seed)((acc, ranges) => {
          var dest = acc
          for (range <- ranges) {
            if (range(1) <= acc && acc <= range(1) + range(2)) {
              dest = range(0) + acc - range(1)
            }
          }
          dest
        })
      }).reduce((a, b) => a.min(b))

      println(minLocation)
    }
  }

  task1()
  task2()
}
