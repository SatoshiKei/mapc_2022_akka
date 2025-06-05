package model
import scala.collection.mutable

case class Coordinate(x: Int, y: Int) {

  def normalize(maxWidth: Option[Int], maxHeight: Option[Int]): Coordinate = {
    val newX = maxWidth.map(mw => ((x % mw) + mw) % mw).getOrElse(x)
    val newY = maxHeight.map(mh => ((y % mh) + mh) % mh).getOrElse(y)
    Coordinate(newX, newY)
  }

  def fromDirection(dir: String): Coordinate = dir match {
    case "n" => Coordinate(0, -1)
    case "s" => Coordinate(0, 1)
    case "e" => Coordinate(1, 0)
    case "w" => Coordinate(-1, 0)
    case _   => Coordinate(0, 0) // Default or invalid direction
  }

  def move(directions: List[String]): Coordinate = {
    directions.foldLeft(this) { case (coord, dir) =>
      dir match {
        case "n" => coord.copy(y = coord.y - 1)
        case "s" => coord.copy(y = coord.y + 1)
        case "e"  => coord.copy(x = coord.x + 1)
        case "w"  => coord.copy(x = coord.x - 1)
      }
    }
  }

  def +(other: Coordinate): Coordinate = Coordinate(x + other.x, y + other.y)

  def -(other: Coordinate): Coordinate = Coordinate(x - other.x, y - other.y)

  def neighbors(range: Int = 1, includeDiagonals: Boolean = true): List[Coordinate] = {
    (for {
      dx <- -range to range
      dy <- -range to range
      if (dx != 0 || dy != 0) &&
        (includeDiagonals || dx == 0 || dy == 0)
    } yield Coordinate(x + dx, y + dy)).toList
  }

  // Given a neighboring coordinate, returns the direction string (e.g. "n", "s", "e", "w")
  def toDirection(to: Coordinate): Option[String] = {
    val dx = to.x - this.x
    val dy = to.y - this.y

    (dx, dy) match {
      case (0, -1) => Some("n")
      case (0, 1) => Some("s")
      case (1, 0) => Some("e")
      case (-1, 0) => Some("w")
      case _ => None
    }
  }

  def toDirection: Option[String] = {
    (x, y) match {
      case (0, -1) => Some("n")
      case (0, 1)  => Some("s")
      case (1, 0)  => Some("e")
      case (-1, 0) => Some("w")
      case _       => None
    }
  }


  def rotateToFacing(facing: String): Coordinate = facing match {
    case "n" => this
    case "e" => Coordinate(-y, x)
    case "s" => Coordinate(-x, -y)
    case "w" => Coordinate(y, -x)
  }

  def getOrderedPerpendiculars: (Coordinate, Coordinate) = this match {
    case Coordinate(0, -1) => (Coordinate(-1, 0), Coordinate(1, 0)) // Facing north: left=w, right=e
    case Coordinate(0, 1)  => (Coordinate(1, 0), Coordinate(-1, 0)) // Facing south: left=e, right=w
    case Coordinate(-1, 0) => (Coordinate(0, 1), Coordinate(0, -1)) // Facing west: left=s, right=n
    case Coordinate(1, 0)  => (Coordinate(0, -1), Coordinate(0, 1)) // Facing east: left=n, right=s
    case _ => (Coordinate(0, 0), Coordinate(0, 0)) // Invalid
  }


  def mostFavorableNeighbor(globalMap: mutable.Map[Coordinate, Thing]): Option[Coordinate] = {
    val priorities = List("empty", "obstacle", "entity", "dispenser")

    val sorted = this.neighbors(1, includeDiagonals = false).sortBy { coord =>
      globalMap.get(coord).map(_.`type`) match {
        case Some(typ) if priorities.contains(typ) => priorities.indexOf(typ)
        case _ => priorities.length
      }
    }

    sorted.headOption
  }


  def getClosestCoordByDistanceByTwoCoordsLine(start: Coordinate, end: Coordinate, distance: Int): Coordinate = {
    val dx = start.x - end.x
    val dy = start.y - end.y
    val norm = math.sqrt(dx * dx + dy * dy)

    val newX = end.x - distance * dx / norm
    val newY = end.y - distance * dy / norm

    Coordinate(Math.round(newX).toInt, Math.round(newY).toInt)
  }

  def manhattanDistance(other: Coordinate): Int =
    math.abs(x - other.x) + math.abs(y - other.y)

  def toRelative(target: Coordinate): Coordinate = target - this

  def distanceTo(other: Coordinate): Double =
    math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))

  def isAdjacentTo(other: Coordinate): Boolean = {
    val dx = Math.abs(this.x - other.x)
    val dy = Math.abs(this.y - other.y)
    dx + dy == 1
  }

  def euclideanDistance(other: Coordinate): Double =
    math.hypot(x - other.x, y - other.y)

  def chebyshevDistance(other: Coordinate): Int =
    math.max(math.abs(x - other.x), math.abs(y - other.y))

  def *(scalar: Int): Coordinate = Coordinate(x * scalar, y * scalar)


  override def toString: String = s"($x, $y)"
}

//object Coordinate {
//  val Origo: Coordinate = Coordinate(0, 0)
//
//  def fromTuple(t: (Int, Int)): Coordinate = Coordinate(t._1, t._2)
//
//  def relativeOffset(from: Coordinate, to: Coordinate): Coordinate =
//    Coordinate(to.x - from.x, to.y - from.y)
//
//}


