package model
import scala.collection.mutable

case class Observation(
                        agentId: String,
                        currentPos: Coordinate,
                        orientation: String,
                        energy: Int,
                        things: Vector[Thing],
                        attached: Vector[Coordinate],
                        visionRadius: Int,
                        roles: Vector[Role],
                        currentRole: Option[String],
                        simulation: Simulation,
                        globalMap: mutable.Map[Coordinate, String]
                      ) {

  def hasRole(roleName: String): Boolean = {
    roles.exists(_.name == roleName)
  }

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
      val rotated = relative.rotateToFacing(orientation)
      val absolute = currentPos + rotated
      globalMap.update(absolute, thing.`type`) // Always update; latest observation is most accurate
    }

    // Also record the agent's own current position as "entity"
    globalMap.update(currentPos, "entity")

    // Mark empty tiles within vision that are not occupied by anything
    for {
      dx <- -visionRadius to visionRadius
      dy <- -visionRadius to visionRadius
      if math.abs(dx) + math.abs(dy) <= visionRadius
    } {
      val rel = Coordinate(dx, dy)
      val abs = currentPos + rel.rotateToFacing(orientation)
      if (!things.exists(t => t.x == dx && t.y == dy)) {
        globalMap.update(abs, "empty")
      }
    }
  }




  def isUnknown(coord: Coordinate): Boolean = {
    !globalMap.contains(coord)
  }

  def findClosestUnknownInMap(maxDistance: Int = 20): Option[Coordinate] = {
    val visited = scala.collection.mutable.Set[Coordinate]()
    val queue = scala.collection.mutable.Queue[(Coordinate, Int)]()

    queue.enqueue((currentPos, 0))
    visited.add(currentPos)

    while (queue.nonEmpty) {
      val (coord, dist) = queue.dequeue()
      if (dist > maxDistance) return None
      if (!globalMap.contains(coord)) return Some(coord)
      for (n <- coord.neighbors() if !visited.contains(n)) {
        visited.add(n)
        queue.enqueue((n, dist + 1))
      }
    }
    None
  }

  def findClosestUnknownFromStartingLocation(startCoordinate: Coordinate, currentCoordinate: Coordinate, vision: Int): Option[Coordinate] = {

    var rangeIncrement = 0
    var iterCount = 0
    var maxIterations = 60

    // Initial vision ring
    var unknownCoordinates = currentCoordinate.neighbors(vision).filter(coord => !globalMap.contains(coord))

    // Spiral out until we find unknowns or exceed max iterations
    while (unknownCoordinates.isEmpty && iterCount <= maxIterations) {
      iterCount += 1
      rangeIncrement += 3

      unknownCoordinates = currentCoordinate.neighbors(vision+rangeIncrement).filter(coord => !globalMap.contains(coord))
    }

    if (iterCount > maxIterations || unknownCoordinates.isEmpty)
      return None

    // Shuffle unknowns to randomize agent directions
    val shuffled = scala.util.Random.shuffle(unknownCoordinates)

    // Find the coordinate(s) closest to the starting position
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
    things.collect {
      case t if t.`type` == "dispenser" && t.details == blockType =>
        val relative = Coordinate(t.x, t.y).rotateToFacing(orientation)
        currentPos + relative
    }.sortBy(_.manhattanDistance(currentPos)).headOption
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
    globalMap.get(coord) match {
      case Some(value) =>
        val impassables = Set("block", "obstacle") // Exclude "entity"
        !impassables.contains(value.toLowerCase)
      case None =>
        true // Unknown tiles are considered passable
    }
  }

  def getClosestFreeGoalZoneForTask(
                                     currentPos: Coordinate,
                                     blockRelCoords: Seq[Coordinate],
                                     goalZones: Set[Coordinate],
                                     globalMap: mutable.Map[Coordinate, String]
                                   ): Option[Coordinate] = {
    val occupied = Set("agent", "block", "marker", "dispenser")

    def isZoneFree(center: Coordinate): Boolean = {
      val absoluteCoords = blockRelCoords.map(center + _)
      val surrounding = absoluteCoords.flatMap(_.neighbors(includeDiagonals = false))
      val allToCheck = Seq(center) ++ absoluteCoords ++ surrounding

      allToCheck.forall(c => globalMap.get(c).forall(v => !occupied.contains(v)))
    }

    goalZones
      .filter(isZoneFree)
      .toSeq
      .sortBy(_.distanceTo(currentPos))
      .headOption
  }




}