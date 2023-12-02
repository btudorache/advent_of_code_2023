case class GameHand(value: Int, game_type: String)

object Main extends App {
  val task1 = () => {
    val MAX_RED = 12
    val MAX_GREEN = 13
    val MAX_BLUE = 14

    val res = scala.io.Source.fromFile("input").getLines.foldLeft(0)((acc: Int, curr: String) => {
      var valid_game = true

      curr.split(": ")(1).split("; ").foreach { game_round =>
        var curr_red = 0
        var curr_green = 0
        var curr_blue = 0

        game_round.split(", ").foreach { game_hand =>
          val args = game_hand.split(" ")
          val game_hand_instance = GameHand(args(0).toInt, args(1))
          game_hand_instance match {
            case GameHand(value, "red") => curr_red = value
            case GameHand(value, "green") => curr_green = value
            case GameHand(value, "blue") => curr_blue = value
          }
        }

        if (curr_red > MAX_RED || curr_green > MAX_GREEN || curr_blue > MAX_BLUE) {
          valid_game = false
        }
      }

      val game_number = curr.split(": ")(0).split(" ")(1).toInt
      if (valid_game) {
        acc + game_number
      } else {
        acc
      }
    })

    println(res)
  }

  val task2 = () => {
    val res = scala.io.Source.fromFile("input").getLines.foldLeft(0)((acc: Int, curr_game: String) => {
      var max_red = 0
      var max_green = 0
      var max_blue = 0

      curr_game.split(": ")(1).split("; ").foreach { game_round =>
        game_round.split(", ").foreach { game_hand =>
          val args = game_hand.split(" ")
          val game_hand_instance = GameHand(args(0).toInt, args(1))
          game_hand_instance match {
            case GameHand(value, "red") => max_red = max_red.max(value)
            case GameHand(value, "green") => max_green = max_green.max(value)
            case GameHand(value, "blue") => max_blue = max_blue.max(value)
          }
        }
      }

      acc + max_red * max_green * max_blue
    })

    println(res)
  }

  task1()
  task2()
}
