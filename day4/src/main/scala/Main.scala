import scala.util.Using

object Main extends App {
  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val res = source.getLines().foldLeft(0)((acc, line) => {
        val sets = line.split(": ")(1).split(" \\| ")
        val sortedWinner = sets(0).split(" ")
          .filter(str => str != "")
          .map(str => str.strip().toInt).sorted

        val winningMatches = sets(1).stripLeading().split(" ")
          .filter(str => str != "")
          .map(str => str.strip().toInt)
          .filter(currMatch => {
            val insertionPoint = sortedWinner.search(currMatch).insertionPoint
            if (insertionPoint < sortedWinner.length && sortedWinner(insertionPoint) == currMatch) true else false
          })
          .foldLeft(0)((acc, _) => if (acc == 0) 1 else acc * 2)
        acc + winningMatches
      })

      println(res)
    }
  }


  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val ticketsMap = scala.collection.mutable.Map[Int, Int]()
      for (x <- 1 to lines.length) {
        ticketsMap(x) = 1
      }

      lines.zipWithIndex.foreach { case (line, index) =>
        val sets = line.split(": ")(1).split(" \\| ")
        val sortedWinner = sets(0).split(" ")
          .filter(str => str != "")
          .map(str => str.strip().toInt).sorted

        val winningMatches = sets(1).stripLeading().split(" ")
          .filter(str => str != "")
          .map(str => str.strip().toInt).count(currMatch => {
            val insertionPoint = sortedWinner.search(currMatch).insertionPoint
            if (insertionPoint < sortedWinner.length && sortedWinner(insertionPoint) == currMatch) true else false
          })

        for (x <- index + 2 to index + winningMatches + 1) {
          if (ticketsMap.isDefinedAt(x)) {
            ticketsMap(x) += ticketsMap(index + 1)
          }
        }
      }

      println(ticketsMap.values.sum)
    }
  }

  task1()
  task2()
}
