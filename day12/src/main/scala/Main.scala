import scala.collection.mutable.ArrayBuffer
import scala.util.Using

object Main extends App {
  def generateArrangements(currSol: ArrayBuffer[Char], remainingBroken: Int, wildcardPositions: ArrayBuffer[Int], currPos: Int, brokenList: Array[Int], solutions: ArrayBuffer[List[Char]]): Unit = {
    if (currPos == wildcardPositions.length) {
      val filtered = currSol.mkString("").split("\\.+").filter(str => str != "").map(_.length)
      if (filtered.sameElements(brokenList)) {
        solutions.addOne(currSol.clone().toList)
      }
      return
    }

    val currentTaken = currSol.takeWhile(p => p != '?').mkString("").split("\\.+").filter(str => str != "").map(_.length)
    for (ind <- 0 until currentTaken.length.min(brokenList.length)) {
      if ( currentTaken(ind) > brokenList(ind)) {
        return
      }
    }

    val options = if (remainingBroken > 0) List('#', '.') else List('.')
    for (x <- options) {
      currSol(wildcardPositions(currPos)) = x
      generateArrangements(currSol, if (x == '#') remainingBroken - 1 else remainingBroken, wildcardPositions, currPos + 1, brokenList, solutions)
      currSol(wildcardPositions(currPos)) = '?'
    }
  }

  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val numArrangements = lines.map(line => {
        val args = line.split(" ")
        val arrangement = args.head.toList
        val brokenList = args(1).split(",").map(_.toInt)

        var currentBroken = 0
        val wildcardPositions = scala.collection.mutable.ArrayBuffer[Int]()
        for (i <- arrangement.indices) {
          arrangement(i) match {
            case '#' => currentBroken += 1
            case '?' => wildcardPositions += i
            case _ =>
          }
        }

        val remainingBroken = brokenList.sum - currentBroken
        val solutions = scala.collection.mutable.ArrayBuffer[List[Char]]()
        generateArrangements(ArrayBuffer(arrangement: _*), remainingBroken, wildcardPositions, 0, brokenList, solutions)
        solutions.length
      }).sum

      println(numArrangements)
    }
  }

  // besides some sort of pruning I got no idea
  // apparently it's dp
  val task2 = () => {
    Using(scala.io.Source.fromFile("dummy_input")) { source =>
      val lines = source.getLines().toList
      val numArrangements = lines.map(line => {
        val args = line.split(" ")
        val arrangement = List.fill(5)(args.head).mkString("?")
        val brokenList = List.fill(5)(args(1)).mkString(",").split(",").map(_.toInt)

        var currentBroken = 0
        val wildcardPositions = scala.collection.mutable.ArrayBuffer[Int]()
        for (i <- arrangement.indices) {
          arrangement(i) match {
            case '#' => currentBroken += 1
            case '?' => wildcardPositions += i
            case _ =>
          }
        }

        val remainingBroken = brokenList.sum - currentBroken
        val solutions = scala.collection.mutable.ArrayBuffer[List[Char]]()
        generateArrangements(ArrayBuffer(arrangement: _*), remainingBroken, wildcardPositions, 0, brokenList, solutions)
        println(solutions.length)
        solutions.length
      }).sum

      println(numArrangements)
    }
  }

  task1()
//  task2()
}
