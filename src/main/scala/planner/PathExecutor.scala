package planner

import action.{ClearAction, MoveAction, RotateAction, SkipAction}
import intentions.ClearUnitIntention
import model.{AgentAction, Coordinate, Observation}

import scala.collection.mutable

class PathExecutor(clearPlanner: ClearPlanner = new DefaultClearPlanner()) extends Planner {


  override def nextAction(observation: Observation, target: Coordinate): AgentAction = {

    // STEP 1: If attached to another agent, move out first
    if (observation.currentPos.x == 0 && observation.currentPos.y == 0) {
      val directions = List("n", "e", "s", "w")
      val moveOption = directions.find { dir =>
        val targetCoord = observation.currentPos.fromDirection(dir)
        observation.globalMap.get(targetCoord).forall(v => !Set("block", "obstacle", "entity").contains(v))
      }


      moveOption match {
        case Some(dir) =>
          println(s"[INFO] Detaching from another agent by moving $dir")
          return MoveAction(dir)
        case None =>
          println("[WARN] Cannot detach from another agent — all adjacent tiles blocked.")
          return ClearAction(Coordinate(0, -1))
      }
    }

    findPath(observation.globalMap, observation.currentPos, target, observation.visionRadius) match {
      case Right(nextCoord) =>
        val direction = observation.currentPos.toDirection(nextCoord)
        direction match {
          case Some(desiredDir) =>
            if (desiredDir != observation.orientation) {
              val rotation = computeRotation(observation.orientation, desiredDir)
              println(s"${observation.agentId}'s current orientation: ${observation.orientation}, desired: $desiredDir, target coordinate: " + nextCoord + " rotation: " + rotation)
              return rotation.map(RotateAction(_)).getOrElse(SkipAction())
            } else {
              println(s"${observation.agentId}'s current orientation: ${observation.orientation}, desired: $desiredDir, target coordinate: " + nextCoord)
            }
            clearPlanner.shouldClear(observation, desiredDir).getOrElse(MoveAction(desiredDir))

          case None =>
            println(s"[ERROR] Can't rotate toward $nextCoord"); SkipAction()
        }

      case Left(obstacleCoord) =>
        println(s"[INFO] Path blocked, fallback to ClearIntention at $obstacleCoord")
        // fallback ClearIntention can be created here
        new ClearUnitIntention(obstacleCoord).planNextAction(observation)
    }
  }


  private def findPath(
                        map: mutable.Map[Coordinate, String],
                        start: Coordinate,
                        goal: Coordinate,
                        vision: Int
                      ): Either[Coordinate, Coordinate] = { // Left = obstacle, Right = first step toward goal
    val open = mutable.PriorityQueue[(Coordinate, Int)]()(Ordering.by(-_._2))
    val cameFrom = mutable.Map[Coordinate, Coordinate]()
    val gScore = mutable.Map(start -> 0)
    val fScore = mutable.Map(start -> heuristic(start, goal))
    var firstObstacle: Option[Coordinate] = None

    open.enqueue((start, fScore(start)))

    while (open.nonEmpty) {
      val (current, _) = open.dequeue()

      if (current == goal) return Right(reconstructFirstStep(cameFrom, current, start).get)

      for (n <- current.neighbors(1, includeDiagonals = false)) {
        val blocked = map.get(n).exists(v => Set("obstacle", "block", "entity").contains(v))
        val tentative = gScore(current) + 1

        if (tentative < gScore.getOrElse(n, Int.MaxValue)) {
          cameFrom(n) = current
          gScore(n) = tentative
          fScore(n) = tentative + heuristic(n, goal)
          if (!open.exists(_._1 == n)) open.enqueue((n, fScore(n)))

          if (blocked && firstObstacle.isEmpty)
            firstObstacle = Some(n)
        }
      }
    }

    firstObstacle.toLeft(null)
  }


  private def reconstructFirstStep(cameFrom: mutable.Map[Coordinate, Coordinate], goal: Coordinate, start: Coordinate): Option[Coordinate] = {
    var curr = goal
    if (curr == start) return None
    while (cameFrom.contains(curr) && cameFrom(curr) != start) {
      curr = cameFrom(curr)
    }
    Some(curr).filter(_ != start)
  }

  private def computeRotation(current: String, target: String): Option[String] = {
    val dirs = List("n", "e", "s", "w")
    val currentIdx = dirs.indexOf(current) //2
    val targetIdx = dirs.indexOf(target) //1

    if (currentIdx == -1 || targetIdx == -1) return None

    val diff = (targetIdx - currentIdx + 4) % 4
    diff match {
      case 1 => Some("cw")
      case 2 => Some("cw")
      case 3 => Some("ccw")
      case _ => None // no rotation needed or 180° turn (unsupported)
    }
  }


  private def heuristic(a: Coordinate, b: Coordinate): Int =
    math.abs(a.x - b.x) + math.abs(a.y - b.y)

  private def isPassable(map: mutable.Map[Coordinate, String], coord: Coordinate): Boolean =
    map.get(coord).forall(v => !Set("obstacle", "block").contains(v))


}
