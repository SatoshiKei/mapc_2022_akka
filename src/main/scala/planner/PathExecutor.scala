package planner

import action.{ClearAction, MoveAction, RotateAction, SkipAction}
import intentions.ClearUnitIntention
import model.{AgentAction, Coordinate, Observation, Thing}

import scala.collection.mutable

class PathExecutor(clearPlanner: ClearPlanner = new DefaultClearPlanner()) extends Planner {


  override def nextAction(observation: Observation, target: Coordinate): AgentAction = {

    // STEP 1: If attached to another agent, move out first
    if (observation.things.count(t => t.x == 0 && t.y == 0) > 1) {
      val directions = List("n", "e", "s", "w")
      val moveOption = directions.find { dir =>
        val targetCoord = observation.currentPos.fromDirection(dir)
        observation.getTileType(targetCoord).forall(v => !Set("block", "obstacle", "entity", "dispenser").contains(v))
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
    println(observation.agentId + " is finding a path from " + observation.currentPos + " to " + target)
    findPath(observation.globalMap, observation.currentPos, target, observation.visionRadius) match {
      case Right(nextCoord) =>
        val direction = observation.currentPos.toDirection(nextCoord)
        direction match {
          case Some(desiredDir) =>
//            if (desiredDir != observation.orientation) {
//              val rotation = computeRotation(observation.orientation, desiredDir)
//              println(s"${observation.agentId}'s current orientation: ${observation.orientation}, desired: $desiredDir, target coordinate: " + nextCoord + " rotation: " + rotation)
//              return rotation.map(RotateAction(_)).getOrElse(SkipAction())
//            }
//            println(s"${observation.agentId}'s current orientation: ${observation.orientation}, desired: $desiredDir, target coordinate: " + nextCoord)
//            clearPlanner.shouldClear(observation, desiredDir).getOrElse(MoveAction(desiredDir))
            clearPlanner.shouldClear(observation, desiredDir) match {
              case Some(clearAction) => clearAction
              case None => {
                if (observation.isPassable(nextCoord))
                  MoveAction(desiredDir)
                else {
                  findDodgeDirection(observation).map(MoveAction(_)).getOrElse(SkipAction())
                }
              }
            }

          case None =>
            println(s"[ERROR] Can't rotate toward $nextCoord"); SkipAction()
        }

      case Left(obstacleCoord) =>
        println(s"[INFO] Path blocked, fallback to ClearIntention at $obstacleCoord")
        if (observation.globalMap.get(obstacleCoord).exists(thing => thing.`type` == "entity")) {
          println(s"${observation.agentId} is blocked by another agent — attempting to dodge.")
          findDodgeDirection(observation).map(MoveAction(_)).getOrElse(SkipAction())
        } else {
          new ClearUnitIntention(obstacleCoord).planNextAction(observation)
        }
    }
  }

  def findDodgeDirection(obs: Observation): Option[String] = {
    List("n", "e", "s", "w").find { dir =>
      val coord = obs.currentPos.fromDirection(dir) + obs.currentPos
      println(obs.agentId + " is trying to dodge an unclearable obstacle by going to " + coord)
      obs.globalMap.get(coord).forall(v => v.`type` == "empty" || v.`type` == "unknown")
    }
  }



  private def findPath(
                        map: mutable.Map[Coordinate, Thing],
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

      for (n <- current.neighbors(1, includeDiagonals = false) if isPassable(map, n)) {

        val blocked = map.get(n).exists(v => Set("obstacle", "block").contains(v.`type`))
        val tentative = gScore(current) + 1

        val value = map.get(n)

        if (tentative < gScore.getOrElse(n, 60)) {
          cameFrom(n) = current
          gScore(n) = tentative
          fScore(n) = tentative + heuristic(n, goal)
          if (!open.exists(_._1 == n)) open.enqueue((n, fScore(n)))

          if (blocked && firstObstacle.isEmpty)
            firstObstacle = Some(n)
        }
      }
    }
    println("Start: " + start + " Goal: " + goal + "Came from: " + cameFrom.get(start) + " gScore: " + gScore.get(start) + " fScore: " + fScore.get(start))
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

  private def isPassable(map: mutable.Map[Coordinate, Thing], coord: Coordinate): Boolean =
    map.get(coord).map(_.`type`).forall(v => !Set("entity", "dispenser").contains(v))


}
