import scala.util.Using

object Main extends App {
  val task1 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val hands = scala.collection.mutable.Map[String, Int]()
      lines.foreach(line => {
        val args = line.split(" ").filter(str => str != "")
        hands(args(0)) = args(1).toInt
      })

      val cardValues = Map[Char, Int](('2', 2), ('3', 3), ('4', 4), ('5', 5),
                           ('6', 6), ('7', 7), ('8', 8), ('9', 9),
                           ('T', 10), ('J', 11), ('Q', 12), ('K', 13),
                           ('A', 14))

      val getType = (hand: String) => {
        val letters = scala.collection.mutable.Map[Char, Int]()
        hand.foreach(letter => {
          if (!letters.isDefinedAt(letter)) {
            letters(letter) = 1
          } else {
            letters(letter) += 1
          }
        })

        val keys = letters.keys.toList
        val numKeys = keys.length
        if (numKeys == 1) {
          7
        } else if (numKeys == 2) {
          if (letters(keys(0)) == 4 || letters(keys(1)) == 4) {
            6
          } else {
            5
          }
        } else if (numKeys == 3) {
          if (letters(keys(0)) == 3 || letters(keys(1)) == 3 || letters(keys(2)) == 3) {
            4
          } else {
            3
          }
        } else if (numKeys == 4) {
          2
        } else {
          1
        }
      }

      val sortedHands = hands.keys.toList.sortWith((str1, str2) => {
        val type1 = getType(str1)
        val type2 = getType(str2)
        if (type1 == type2) {
          val diffIndex = (0 to 4).iterator.takeWhile(index => cardValues(str1.charAt(index)) == cardValues(str2.charAt(index))).length
          cardValues(str1.charAt(diffIndex)) < cardValues(str2.charAt(diffIndex))
        } else {
          type1 < type2
        }
      })

      val res = sortedHands.zipWithIndex.map { case (hand, index) =>
        hands(hand) * (index + 1)
      }.sum

      println(res)
    }
  }

  def getType2(hand: String): Int = {
    val letters = scala.collection.mutable.Map[Char, Int]()
    hand.foreach(letter => {
      if (!letters.isDefinedAt(letter)) {
        letters(letter) = 1
      } else {
        letters(letter) += 1
      }
    })

    val keys = letters.keys.toList
    val numKeys = keys.length

    if (letters.isDefinedAt('J')) {
      val filteredByJ = letters.toList.filter(pair => pair._1 != 'J')
      if (filteredByJ.isEmpty) {
        return getType2(hand.replaceAll("J", "A"))
      } else {
        return getType2(hand.replaceAll("J", filteredByJ.maxBy(_._2)._1.toString))
      }
    }

    if (numKeys == 1) {
      7
    } else if (numKeys == 2) {
      if (letters(keys(0)) == 4 || letters(keys(1)) == 4) {
        6
      } else {
        5
      }
    } else if (numKeys == 3) {
      if (letters(keys(0)) == 3 || letters(keys(1)) == 3 || letters(keys(2)) == 3) {
        4
      } else {
        3
      }
    } else if (numKeys == 4) {
      2
    } else {
      1
    }
  }

  val task2 = () => {
    Using(scala.io.Source.fromFile("input")) { source =>
      val lines = source.getLines().toList
      val hands = scala.collection.mutable.Map[String, Int]()
      lines.foreach(line => {
        val args = line.split(" ").filter(str => str != "")
        hands(args(0)) = args(1).toInt
      })

      val cardValues = Map[Char, Int](('J', 2), ('2', 3), ('3', 4), ('4', 5), ('5', 6),
        ('6', 7), ('7', 8), ('8', 9), ('9', 10),
        ('T', 11), ('Q', 12), ('K', 13), ('A', 14))

      val sortedHands = hands.keys.toList.sortWith((str1, str2) => {
        val type1 = getType2(str1)
        val type2 = getType2(str2)
        if (type1 == type2) {
          val diffIndex = (0 to 4).iterator.takeWhile(index => cardValues(str1.charAt(index)) == cardValues(str2.charAt(index))).length
          cardValues(str1.charAt(diffIndex)) < cardValues(str2.charAt(diffIndex))
        } else {
          type1 < type2
        }
      })

      val res = sortedHands.zipWithIndex.map { case (hand, index) =>
        hands(hand) * (index + 1)
      }.sum

      println(res)
    }
  }

  task1()
  task2()
}
