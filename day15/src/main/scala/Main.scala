import scala.util.Using

object Main extends App {
  private def hashAlgorithm(str: String): Int = {
    var value = 0
    for (c <- str) {
      value += c.toInt
      value *= 17
      value = value % 256
    }
    value
  }

  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val sequences = lines.head.split(",")
      val res = sequences.map(seq => hashAlgorithm(seq)).sum
      println(res)
    }
  }

  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val sequences = lines.head.split(",")
      val boxesMap = scala.collection.mutable.Map[Int, scala.collection.mutable.ListBuffer[(String, Int)]]()
      sequences.foreach(sequence => {
        if (sequence.contains("=")) {
          val args = sequence.split("=")
          val pair = (args(0), args(1).toInt)
          val boxNumber = hashAlgorithm(pair._1)
          if (!boxesMap.isDefinedAt(boxNumber)) {
            boxesMap(boxNumber) = scala.collection.mutable.ListBuffer[(String, Int)]()
          }

          val index = boxesMap(boxNumber).map(_._1).indexOf(pair._1)
          if (index == -1) {
            boxesMap(boxNumber).addOne(pair)
          } else {
            boxesMap(boxNumber)(index) = pair
          }

          if (boxesMap(boxNumber).contains(pair)) {
            boxesMap(boxNumber).indexOf(pair)
          }
          // it ends with -
        } else {
          val label = sequence.split("-").head
          val boxNumber = hashAlgorithm(label)
          if (boxesMap.isDefinedAt(boxNumber)) {
            boxesMap(boxNumber) = boxesMap(boxNumber).filter(pair => pair._1 != label)
          }
        }
      })

      val filteredMap = boxesMap.view.filterKeys(index => boxesMap(index).nonEmpty).toMap
      val res = filteredMap.map(mapPair => mapPair._2.zipWithIndex.map { case(lensPair, index) => (mapPair._1 + 1) * (index + 1) * lensPair._2 }.sum).sum
      println(res)
    }
  }

  task1()
  task2()
}
