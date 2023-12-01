object Main extends App {
  val task1 = () => {
    val res = scala.io.Source.fromFile("input").getLines.foldLeft(0)((acc: Int, curr: String) => {
      val first = curr.find(char => char.isDigit).get
      val last = curr.findLast(char => char.isDigit).get
      acc + s"${first}${last}".toInt
    })

    println(res)
  }

  val task2 = () => {
    val res = scala.io.Source.fromFile("input").getLines.foldLeft(0)((acc: Int, curr: String) => {
      val temp = curr
        .replaceAll("one", "o1e")
        .replaceAll("two", "t2o")
        .replaceAll("three", "thr3e")
        .replaceAll("four", "fo4r")
        .replaceAll("five", "fi5e")
        .replaceAll("six", "s6x")
        .replaceAll("seven", "se7en")
        .replaceAll("eight", "eig8t")
        .replaceAll("nine", "ni9e")

      val first = temp.find(char => char.isDigit).get
      val last = temp.findLast(char => char.isDigit).get
      acc + s"${first}${last}".toInt
    })

    println(res)
  }

  task1()
  task2()
}
