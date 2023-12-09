import scala.util.Using

object Main extends App {
  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList

      val generateDifferences = (sequence: Array[Int]) => {
        (1 until sequence.length).toArray.map(index => sequence(index) - sequence(index - 1))
      }

      val res = lines.map(line => {
        val values = line.split(" ").filter(_ != "").map(_.toInt)

        LazyList.iterate(values)(currValue => generateDifferences(currValue))
          .takeWhile(arr => arr.exists(elem => elem != 0))
          .map(arr => arr.last)
          .sum
      }).sum

      println(res)
    }
  }

  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList

      val generateDifferences = (sequence: Array[Int]) => {
        (1 until sequence.length).toArray.map(index => sequence(index) - sequence(index - 1))
      }

      val res = lines.map(line => {
        val values = line.split(" ").filter(_ != "").map(_.toInt)

        LazyList.iterate(values)(currValue => generateDifferences(currValue))
          .takeWhile(arr => arr.exists(elem => elem != 0))
          .map(arr => arr.head)
          .zipWithIndex
          .map { case (element, index) => if (index % 2 == 0) element else -element}
          .sum
      }).sum

      println(res)
    }
  }

  task1()
  task2()
}
