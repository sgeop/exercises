package langton

sealed trait Direction {
  val right: Direction
  val left: Direction
}

case object North extends Direction {
  val right = East
  val left = West
}

case object South extends Direction {
  val right = West
  val left = East
}

case object East extends Direction {
  val right = South
  val left = North
}

case object West extends Direction {
  val right = North
  val left = South
}

case class Location(x: Int, y: Int) {
  def next(direction: Direction) = direction match {
    case North => Location(x, y + 1)
    case South => Location(x, y - 1)
    case East => Location(x + 1, y)
    case West => Location(x - 1, y)
  }
}

case class Ant(direction: Direction, location: Location) {
  def turnRight = Ant(direction.right, location)
  def turnLeft = Ant(direction.left, location)
  
  def move = Ant(direction, location.next(direction))
}

object Game {
  type Board = Set[Location]

  val numSpaces = 10
  val xvals = -(numSpaces / 2) to (numSpaces / 2)
  val yvals = xvals.reverse

  def printBoard(board: Board) = {
    print("\n\n\n\n")
    yvals.map { y =>
      print("\n")
      xvals.map { x =>
        if (board contains Location(x, y)) print(" ") else print("#")
      }
    }
  }

  def run(a0: Ant, b0: Board): Unit = {
    printBoard(b0)
    Thread.sleep(200)
    val a1 = a0.move
    if (b0 contains a1.location) {
      run(a1.turnLeft, b0 - a1.location)
    } else {
      run(a1.turnRight, b0 + a1.location)
    }
  }

  def main(args: Array[String]) {
    run(Ant(North, Location(0, 0)), Set())
  }
}
