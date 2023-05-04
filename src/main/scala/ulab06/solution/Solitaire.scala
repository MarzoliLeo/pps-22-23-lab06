package ulab06.solution

object Solitaire extends App {
  type Position = (Int, Int)
  type Solution = List[Position]

  def render(solution: Seq[Position], width: Int, height: Int): String = {
    val reversed = solution.reverse
    val rows = (0 until height).map { y =>
      (0 until width).map { x =>
        val number = reversed.indexOf((x, y)) + 1
        if number > 0 then f"$number%-2d " else "X  "
      }.mkString
    }
    rows.mkString("\n")
  }

  def placeMarks(width: Int, height: Int)(init: Position): LazyList[Solution] = {
    def _placeMarks(n: Int): LazyList[Solution] = n match {
      case 1 => LazyList(List(init))
      case _ =>
        for {
          solution <- _placeMarks(n - 1)
          newPos <- (0 until width).flatMap { i =>
            (0 until height).map(j => (i, j))
          }.filterNot(solution.contains)
            .filter(isSafe(_, solution.head))
        } yield newPos :: solution
    }

    _placeMarks(width * height)
  }

  def isSafe(p1: Position, p2: Position): Boolean =
    (math.abs(p1._1 - p2._1), math.abs(p1._2 - p2._2)) match {
      case (2, 2) => true
      case (3, 0) => true
      case (0, 3) => true
      case _ => false
    }

  val solutions = placeMarks(5, 7)((2, 3))
  // println(solutions.size) // 13272
  solutions.foreach(s => println(render(s, width = 5, height = 7)))
}
