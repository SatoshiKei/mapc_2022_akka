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
        val targetCoord = Coordinate.fromDirection(dir)
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
      case Right(nextCoord: Coordinate) =>
        println(observation.agentId + " found a path starting with " + nextCoord)
        val direction = observation.currentPos.toDirection(nextCoord)
        direction match {
          case Some(desiredDir) =>
            // Step A: Rotate if carrying a block and not oriented to drag it correctly
            if (observation.attached.nonEmpty) {
              computeMovementWithBlock(observation, nextCoord) match {
                case Some(action: AgentAction) =>
                  return action
                case _ =>
              }
            }

            // Step C: Clear or move
            getCleanOrDodgeAction(observation, desiredDir, nextCoord)

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

  def computeMovementWithBlock(
                                observation: Observation,
                                target: Coordinate
                              ): Option[AgentAction] = {
    val directionOpt = observation.currentPos.toDirection(target)
    if (directionOpt.isEmpty) return None
    val direction = directionOpt.get

    val blockRel = observation.attached.head
    val moveVector = target - observation.currentPos
    val blockAbs = observation.currentPos + blockRel
    val futureBlock = target + blockRel

    val blockIsBehind = blockRel == Coordinate(-moveVector.x, -moveVector.y)
    val targetOk = observation.isEmpty(target) || target == blockAbs
    val blockOk = observation.isEmpty(futureBlock) || futureBlock == observation.currentPos

    if (blockIsBehind && targetOk && blockOk) {
      return Some(MoveAction(direction))
    }

    if (!targetOk && observation.isClearable(target)) {
      return Some(ClearAction(observation.currentPos.toRelative(target)))
    }

    tryRotationThatEnablesMovement(observation, blockRel, moveVector, target)
      .orElse(tryClearingForRotation(observation, blockRel))
  }


  def tryRotationThatEnablesMovement(
                                      observation: Observation,
                                      blockRel: Coordinate,
                                      moveVector: Coordinate,
                                      target: Coordinate
                                    ): Option[AgentAction] = {
    val direction = observation.currentPos.toDirection(target).get
    val rotations = List("cw", "ccw")

    rotations.find { rot =>
      val rotatedRel = blockRel.rotateCoordinate(rot)
      val rotatedAbs = observation.currentPos + rotatedRel

      if (observation.isEmpty(rotatedAbs)) {
        val futureBlock = target + rotatedRel
        val targetOk = observation.isEmpty(target) || target == (observation.currentPos + rotatedRel)
        val blockOk = observation.isEmpty(futureBlock) || futureBlock == observation.currentPos
        val blockWouldBeBehind = rotatedRel == Coordinate(-moveVector.x, -moveVector.y)

        targetOk && blockOk && blockWouldBeBehind
      } else false
    }.map(RotateAction(_))
  }


  def tryClearingForRotation(
                              observation: Observation,
                              blockRel: Coordinate
                            ): Option[AgentAction] = {
    val rotations = List("cw", "ccw")
    rotations.find { rot =>
      val rotatedRel = blockRel.rotateCoordinate(rot)
      val rotatedAbs = observation.currentPos + rotatedRel
      observation.isClearable(rotatedAbs)
    }.map { rot =>
      val rotatedRel = blockRel.rotateCoordinate(rot)
      ClearAction(observation.currentPos.toRelative(observation.currentPos + rotatedRel))
    }
  }



  def getCleanOrDodgeAction(observation: Observation, direction: String, target: Coordinate) = {
    clearPlanner.shouldClear(observation, direction) match {
      case Some(clearAction) => clearAction
      case None =>
        if (observation.isPassable(target))
          MoveAction(direction)
        else {
          findDodgeDirection(observation).map(MoveAction(_)).getOrElse(SkipAction())
        }
    }
  }

  def findDodgeDirection(obs: Observation): Option[String] = {
    List("n", "e", "s", "w").find { dir =>
      val coord = Coordinate.fromDirection(dir) + obs.currentPos
      println(obs.agentId + " is trying to dodge an unclearable obstacle by going to " + coord)
      obs.globalMap.get(coord).forall(v => v.`type` == "empty" || v.`type` == "unknown")
    }
  }

  def handleMoveWhileCarying(observation: Observation, target: Coordinate): Option[AgentAction] = {
    actionToDragBlockBehind(observation, target)
  }

  def actionToDragBlockBehind(observation: Observation, target: Coordinate): Option[AgentAction] = {
    if (observation.attached.isEmpty) return None

    val behind = observation.behind(target)
    val block = observation.attached.head

    println(observation.agentId + " at " + observation.currentPos +  " is carrying a block at " + block + " and pretends to go to target " + target + " after rotating behind " + behind)

    val rotation = (block, behind) match {
      case (b, e) if b == e =>
        println(observation.agentId + " is already behind — no rotation needed")
        None
      case (Coordinate(x, y), Coordinate(x2, y2)) if x == -x2 && y == -y2 =>
        println(observation.agentId +" is opposite to behind — needs 180° rotation")
        Some("180")
      case (Coordinate(x, y), Coordinate(x2, y2)) if math.abs(x - x2) == math.abs(y + y2) =>
        println(observation.agentId + " is perpendicular to behind — needs one rotation")
        Some("90")
      case _ =>
        println(observation.agentId + " could not find a valid rotation")
        None
    }

    val absBehind = observation.currentPos + behind

    rotation match {
      case Some("90") =>
        observation.getTileType(absBehind) match {
          case Some("empty") =>
            Some(RotateAction("cw"))
          case _ =>
            val direction = observation.currentPos.toDirection(absBehind).get //TODO - This could be NONE
            Some(getCleanOrDodgeAction(observation, direction, absBehind))
        }
      case Some("180") =>
        val (left, right) = target.getOrderedPerpendiculars
        val leftTile = observation.currentPos + left
        val rightTile = observation.currentPos + right

        val canRotateLeft = observation.getTileType(leftTile).contains("empty")
        val canRotateRight = observation.getTileType(rightTile).contains("empty")

        (canRotateLeft, canRotateRight) match {
          case (true, false) => Some(RotateAction("ccw"))
          case (false, true) => Some(RotateAction("cw"))
          case (true, true)  => Some(RotateAction("cw"))
          case _ =>
            val direction = observation.currentPos.toDirection(leftTile).get //TODO - This could be NONE : but how? It should always be within map range
            Some(getCleanOrDodgeAction(observation, direction, leftTile))
        }
      case None =>
        None
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

      for (n <- current.neighbors(1, includeDiagonals = false) if canMoveOrClear(map, n)) {

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

  private def canMoveOrClear(map: mutable.Map[Coordinate, Thing], target: Coordinate): Boolean =
    map.get(target).map(_.`type`).forall(v => !Set("entity", "dispenser").contains(v))


}
