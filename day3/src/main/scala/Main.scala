case class MatrixBox(value: Char, box_type: String, var number_value: Int = 0, var marked: Int = 0)

object Main extends App {
  val task1 = () => {
    // This is bad but I'll find a better solution next time to get the number of columns
    val dummy_lines = scala.io.Source.fromFile("input").getLines()
    val numLines = dummy_lines.length
    val numColumns = scala.io.Source.fromFile("input").getLines().next().length

    val arr: Array[Array[MatrixBox]] = Array.ofDim[MatrixBox](numLines, numColumns)

    scala.io.Source.fromFile("input").getLines().zipWithIndex foreach { case(line, line_index) =>
      var curr_number = ""

      line.zipWithIndex foreach { case(col, col_index) =>
        if (col.isDigit) {
          curr_number += col
          arr(line_index)(col_index) = MatrixBox(col, "number")
        } else {
          if (col == '.') {
            arr(line_index)(col_index) = MatrixBox(col, "dot")
          } else {
            arr(line_index)(col_index) = MatrixBox(col, "symbol")
          }

          if (curr_number.nonEmpty) {
            var iterator = col_index - 1
            val target_node = arr(line_index)(iterator)
            target_node.number_value = curr_number.toInt
            while (iterator > 0 && arr(line_index)(iterator).box_type == "number") {
              arr(line_index)(iterator) = target_node
              iterator -= 1
            }
            curr_number = ""
          }
        }
      }

      if (curr_number.nonEmpty) {
        var iterator = numColumns - 1 - 1
        val target_node = arr(line_index)(iterator)
        target_node.number_value = curr_number.toInt
        while (iterator > 0 && arr(line_index)(iterator).box_type == "number") {
          arr(line_index)(iterator) = target_node
          iterator -= 1
        }
      }
    }

    val isValid = (x: Int, y: Int, arr: Array[Array[MatrixBox]]) => if (x >= 0 && x <= numLines - 1 && y >= 0 && y < numColumns - 1 && arr(x)(y).box_type == "number" && arr(x)(y).marked == 0) true else false

    var total_value = 0
    for ((line, line_index) <- arr.zipWithIndex) {
      for (col_index <- line.indices) {
        if (arr(line_index)(col_index).box_type == "symbol") {
          val directions = Array(-1, 0, 1)
          for (x <- directions) {
            for (y <- directions) {
              if (isValid(line_index + x, col_index + y, arr)) {
                total_value += arr(line_index + x)(col_index + y).number_value
                arr(line_index + x)(col_index + y).marked = 1
              }
            }
          }
        }
      }
    }

    println(total_value)
  }

  val task2 = () => {
    // This is bad but I'll find a better solution next time to get the number of columns
    val dummy_lines = scala.io.Source.fromFile("input").getLines()
    val numLines = dummy_lines.length
    val numColumns = scala.io.Source.fromFile("input").getLines().next().length

    val arr: Array[Array[MatrixBox]] = Array.ofDim[MatrixBox](numLines, numColumns)

    scala.io.Source.fromFile("input").getLines().zipWithIndex foreach { case (line, line_index) =>
      var curr_number = ""

      line.zipWithIndex foreach { case (col, col_index) =>
        if (col.isDigit) {
          curr_number += col
          arr(line_index)(col_index) = MatrixBox(col, "number")
        } else {
          if (col == '.') {
            arr(line_index)(col_index) = MatrixBox(col, "dot")
          } else {
            arr(line_index)(col_index) = MatrixBox(col, "symbol")
          }

          if (curr_number.nonEmpty) {
            var iterator = col_index - 1
            val target_node = arr(line_index)(iterator)
            target_node.number_value = curr_number.toInt
            while (iterator > 0 && arr(line_index)(iterator).box_type == "number") {
              arr(line_index)(iterator) = target_node
              iterator -= 1
            }
            curr_number = ""
          }
        }
      }

      if (curr_number.nonEmpty) {
        var iterator = numColumns - 1 - 1
        val target_node = arr(line_index)(iterator)
        target_node.number_value = curr_number.toInt
        while (iterator > 0 && arr(line_index)(iterator).box_type == "number") {
          arr(line_index)(iterator) = target_node
          iterator -= 1
        }
      }
    }

    val isValid = (x: Int, y: Int, arr: Array[Array[MatrixBox]]) => if (x >= 0 && x <= numLines - 1 && y >= 0 && y < numColumns - 1 && arr(x)(y).box_type == "number" && arr(x)(y).marked == 0) true else false

    var total_value = 0
    for ((line, line_index) <- arr.zipWithIndex) {
      for (col_index <- line.indices) {
        if (arr(line_index)(col_index).box_type == "symbol" && arr(line_index)(col_index).value == '*') {
          var num_gears = 0
          var product = 1
          val directions = Array(-1, 0, 1)
          for (x <- directions) {
            for (y <- directions) {
              if (isValid(line_index + x, col_index + y, arr)) {
                num_gears += 1
                product *= arr(line_index + x)(col_index + y).number_value
                arr(line_index + x)(col_index + y).marked = 1
              }
            }
          }

          if (num_gears == 2) {
            total_value += product
          }
        }
      }
    }

    println(total_value)
  }

  task1()
  task2()
}
