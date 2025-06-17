package model
import shared.KnownAgent

import scala.collection.mutable

case class Observation(
                        agentId: String,
                        currentPos: Coordinate,
                        energy: Int,
                        things: Vector[Thing],
                        attached: Vector[Coordinate],
                        currentRole: Option[String],
                        simulation: Simulation,
                        globalMap: mutable.Map[Coordinate, Thing],
                        goalZones: Set[Coordinate],
                        knownGoalZones: mutable.Map[Coordinate, Zone],
                        knownRoleZones: mutable.Map[Coordinate, Zone],
                        taskRegistry: mutable.Map[String, TaskTeamRegistry] = mutable.Map.empty,
                        knownAgents: mutable.Map[String, KnownAgent] = mutable.Map.empty
                      ) {

  def getBlockedDirections: Set[String] = {
    val directionOffsets = Map(
      "n" -> (0, -1),
      "s" -> (0, 1),
      "e" -> (1, 0),
      "w" -> (-1, 0)
    )

    directionOffsets.collect {
      case (dir, (dx, dy)) if things.exists(t =>
        t.x == dx && t.y == dy && (t.`type` == "obstacle" || t.`type` == "entity")
      ) => dir
    }.toSet
  }

  def updateKnownMap(): Unit = {
    for (thing <- things) {
      val relative = Coordinate(thing.x, thing.y)
      val absolute = currentPos + relative
      globalMap.update(absolute, thing)
    }

    // Mark empty tiles within vision that are not occupied by anything
    for {
      dx <- -visionRadius to visionRadius
      dy <- -visionRadius to visionRadius
      if math.abs(dx) + math.abs(dy) <= visionRadius
    } {
      val rel = Coordinate(dx, dy)
      val abs = currentPos + rel
      if (!things.exists(t => t.x == dx && t.y == dy) && math.abs(dx) + math.abs(dy) <= visionRadius) {
        globalMap.update(abs, Thing(dx, dy, "empty", "", simulation.getSimulationStep))
      }

      //Goal zones can move, so set them as inactive if they are present in the zone map, but vanished from the local percept
      knownGoalZones.get(abs) match {
        case Some(zone) if zone.active && !goalZones.contains(abs) =>
          knownGoalZones.update(abs, zone.copy(simulation.getSimulationStep, active = false))
        case _ =>
      }
    }
    println(agentId + " Role Zones: " + getKnownRoleZones.size + " Goal Zones: " + getKnownGoalZones.size + " Global Map: " + globalMap.size + " Step: " + simulation.getSimulationStep + " Dispensers: " + knownDispenserSummary())
  }

//  def visionRadius: Int = {
//    simulation.getAllRoles.find(role => role.name == currentRole.get).get.vision
//  }

  def visionRadius: Int = {
    val allRoles = simulation.getAllRoles
    def getVision(roleName: String): Option[Int] = {
      allRoles.find(_.name == roleName).flatMap(role => Option(role.vision))
    }
    currentRole.flatMap(getVision).orElse(getVision("default")).getOrElse(5) //TODO - Find out why it's null sometimes, it should always fallback to "default"
  }


  def getKnownRoleZones: Set[Coordinate] = {
    knownRoleZones.collect {
      case (coord, zone) if zone.active => coord
    }.toSet
  }

  def getKnownGoalZones: Set[Coordinate] = {
    knownGoalZones.collect {
      case (coord, zone) if zone.active => coord
    }.toSet
  }


  def isUnknown(coord: Coordinate): Boolean = {
    !globalMap.contains(coord)
  }

  def getRemoteCoordinate(shared: SharedCoordinate) = {
    val offset = knownAgents(shared.system).offset
    Coordinate(shared.x + offset.x, shared.y + offset.y)
  }

  def isBlockAttachedAt(position: Coordinate, blockType: String): Boolean = {
    attached.exists { relCoord =>
      relCoord == position && things.exists { t =>
        Coordinate(t.x, t.y) == position &&
          t.`type` == "block" &&
          t.details == blockType
      }
    }
  }

  def isBlockAttached(blockType: String): Boolean = {
    attached.exists { relCoord =>
      things.exists { t =>
          t.`type` == "block" &&
          t.details == blockType
      }
    }
  }

  def behind(target: Coordinate): Coordinate = {
    val dx = target.x - currentPos.x
    val dy = target.y - currentPos.y

    Coordinate( - dx,  - dy)
  }

  def translateRemoteCoordinate(shared: SharedCoordinate): Option[Coordinate] = {
    if (shared.system == agentId) {
      Coordinate(shared.x, shared.y)
    }
    if (knownAgents.contains(shared.system)) {
      val agent = knownAgents(shared.system)
      Some(Coordinate(shared.x + agent.offset.x, shared.y + agent.offset.y))
    }
    else if (agentId == shared.system) {
        Some(Coordinate(shared.x, shared.y))
    } else {
      println(agentId +  " does not know " + shared.system)
     None
    }
  }

  def allRequirementsMet(task: Task): Boolean = {
    val attached: Map[Coordinate, String] = this.attached.flatMap { relCoord =>
      globalMap.get(currentPos + relCoord).map(_.details).map(t => relCoord -> t)
    }.toMap

    task.requirements.forall { req =>
      attached.get(Coordinate(req.coordinate.x, req.coordinate.y)).contains(req.`type`)
    }
  }


  def canMove(direction: String, currentPos: Coordinate, attached: Seq[Coordinate], globalMap: mutable.Map[Coordinate, Thing]): Boolean = {
    val targetCoord = Coordinate.fromDirection(direction) + currentPos

    if (attached.isEmpty) {
      // No block attached: just check the destination tile
      return globalMap.get(targetCoord).forall(_.`type` == "empty")
    }

    val blockRel = attached.head
    val newBlockCoord = targetCoord + blockRel

    val targetTileIsEmpty = globalMap.get(targetCoord).forall(_.`type` == "empty")
    val blockTileIsEmpty = globalMap.get(newBlockCoord).forall(_.`type` == "empty")

    targetTileIsEmpty && blockTileIsEmpty
  }





//  def findClosestUnknownInMap(maxDistance: Int = 20): Option[Coordinate] = {
//    val visited = scala.collection.mutable.Set[Coordinate]()
//    val queue = scala.collection.mutable.Queue[(Coordinate, Int)]()
//
//    queue.enqueue((currentPos, 0))
//    visited.add(currentPos)
//
//    while (queue.nonEmpty) {
//      val (coord, dist) = queue.dequeue()
//      if (dist > maxDistance) return None
//      if (!globalMap.contains(coord)) return Some(coord)
//      for (n <- coord.neighbors() if !visited.contains(n)) {
//        visited.add(n)
//        queue.enqueue((n, dist + 1))
//      }
//    }
//    None
//  }

  def findClosestUnknownFromStartingLocation(startCoordinate: Coordinate, currentCoordinate: Coordinate, vision: Int): Option[Coordinate] = {

    var rangeIncrement = 0
    var iterCount = 0
    var maxIterations = 50

    var unknownCoordinates = currentCoordinate.neighbors(vision).filter(coord => !globalMap.contains(coord))

    while (unknownCoordinates.isEmpty && iterCount <= maxIterations) {
      iterCount += 1
      rangeIncrement += 3

      unknownCoordinates = currentCoordinate.neighbors(vision+rangeIncrement).filter(coord => !globalMap.contains(coord))
    }

    if (iterCount > maxIterations || unknownCoordinates.isEmpty)
      return None

    val shuffled = scala.util.Random.shuffle(unknownCoordinates)

    val minDist = shuffled.map(startCoordinate.distanceTo).min
    val candidates = shuffled.filter(c => math.abs(startCoordinate.distanceTo(c) - minDist) < 0.1)

    val closest = candidates.minBy(currentCoordinate.distanceTo)

    val offsetCoord = closest.getClosestCoordByDistanceByTwoCoordsLine(
      start = currentCoordinate,
      end = closest,
      distance = vision
    )

    if (!globalMap.contains(offsetCoord))
      Some(offsetCoord)
    else
      Some(closest)
  }


  def findClosestDispenser(blockType: String): Option[Coordinate] = {
    globalMap.collect {
      case (coord, value) if value.`type` == "dispenser" && getTileDetail(coord).contains(blockType) =>
        coord
    }.toSeq.sortBy(_.manhattanDistance(currentPos)).headOption
  }

  def getTileType(coord: Coordinate): Option[String] =
    globalMap.get(coord).map(_.`type`)

  def getTileDetail(coord: Coordinate): Option[String] =
    globalMap.get(coord).map(_.details)

  def isBlockOfType(coord: Coordinate, blockType: String): Boolean = {
    globalMap.get(coord).exists { thing =>
      thing.`type` == "block" && thing.details == blockType
    }
  }


  def knownDispenserSummary(): String = {
    val blockTypes = globalMap.collect {
      case (_, value) if value.`type` == "dispenser" =>
        value.details
    }

    val b0 = blockTypes.count(_ == "b0")
    val b1 = blockTypes.count(_ == "b1")
    val b2 = blockTypes.count(_ == "b2")

    s"$agentId known dispensers — b0: $b0, b1: $b1, b2: $b2"
  }



  //TO DO
  def mapIsFullyExplored: Boolean = false

  def findRandomFarCoordinate(): Coordinate = {
    val distance = visionRadius * 4
    val minDistance = (visionRadius * 3) max 1
    val range = distance + 1

    // Generate candidate coordinates around the agent
    val candidates = for {
      dx <- -range to range
      dy <- -range to range
      if {
        val dist = math.abs(dx) + math.abs(dy)
        dist >= minDistance && dist <= distance
      }
      coord = Coordinate(currentPos.x + dx, currentPos.y + dy)
      if isPassable(coord) && isUnknown(coord)
    } yield coord

    scala.util.Random.shuffle(candidates).headOption.get
  }


  def isPassable(coord: Coordinate): Boolean = {
    getTileType(coord) match {
      case Some(value) =>
        val impassables = Set("block", "obstacle", "entity", "dispenser")
        !impassables.contains(value.toLowerCase)
      case None =>
        true // Unknown tiles are considered passable
    }
  }

  def isEmpty(coord: Coordinate): Boolean = {
    getTileType(coord).contains("empty")
  }

  def isClearable(coord: Coordinate): Boolean = {
    val clearable = Set("block", "obstacle")
    val rel = coord - currentPos
    val adjacent = Set(Coordinate(0, 1), Coordinate(0, -1), Coordinate(1, 0), Coordinate(-1, 0))
    getTileType(coord).exists(clearable.contains) && adjacent.contains(rel)
  }



}