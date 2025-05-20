package intentions

import model._

class ExploreIntention() extends Intention {

  private var currentTravelIntention: Option[TravelIntention] = None
  private var detachBlocksIntention: Option[DetachBlocksIntention] = None
  private var adoptRoleIntention: Option[AdoptRoleIntention] = None
  private var relocating: Boolean = false

  override def planNextAction(observation: Observation): AgentAction = {
    // 1. Detach if carrying too many blocks
    val maxBlocks = observation.simulation.getMaxBlockRegulation
    val attached = observation.attached
    if (attached.size > 1 || maxBlocks.exists(_ < attached.size)) {
      if (detachBlocksIntention.isEmpty) {
        detachBlocksIntention = Some(new DetachBlocksIntention())
      }
      return detachBlocksIntention.get.planNextAction(observation)
    }

    // 2. Adopt role if needed
    val reservedRole = observation.simulation.getReservedRoles().get(observation.agentId)
    if (reservedRole.isDefined && !observation.currentRole.contains(reservedRole.get.name)) {
      if (adoptRoleIntention.isEmpty) {
        adoptRoleIntention = Some(new AdoptRoleIntention(reservedRole.get.name))
      }
      return adoptRoleIntention.get.planNextAction(observation)
    }

    // 3. Determine new travel target if needed
    if (
      currentTravelIntention.isEmpty ||
        currentTravelIntention.exists(_.checkFinished(observation)) ||
        (!relocating && !observation.isUnknown(currentTravelIntention.get.target))
    ) {
      relocating = false

      val maybeTarget = observation.findClosestUnknownFromStartingLocation(new Coordinate(0,0), observation.currentPos, observation.visionRadius)
      val target = maybeTarget.getOrElse {
        relocating = true
        observation.findRandomFarCoordinate()
      }
      println(observation.agentId + " exploring target: " + target)

      currentTravelIntention = Some(new TravelIntention(target))
    }

    // 4. Execute travel
    currentTravelIntention.get.planNextAction(observation)
  }

  override def checkFinished(observation: Observation): Boolean = {
    observation.simulation.getMapCount.contains(1) && observation.mapIsFullyExplored
  }

  override def shouldAbort(observation: Observation): Boolean = {
    observation.getBlockedDirections.size == 4
  }

  private def findNextSpiralUnknown(observation: Observation, maxRadius: Int = 20): Option[Coordinate] = {
    val origin = Coordinate(0, 0)
    var x = 0
    var y = 0
    var dx = 0
    var dy = -1
    var step = 0

    for (_ <- 0 until (2 * maxRadius + 1) * (2 * maxRadius + 1)) {
      val coord = Coordinate(x, y)
      if (coord.manhattanDistance(origin) <= maxRadius && observation.isUnknown(coord)) {
        return Some(coord)
      }

      // Turn at corners
      if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
        val tmp = dx
        dx = -dy
        dy = tmp
      }

      x += dx
      y += dy
    }

    None
  }




  //  override def updateCoordinatesByOffset(offset: Coordinate): Unit = {
//    currentTravelIntention.foreach(_.updateCoordinatesByOffset(offset))
//    detachBlocksIntention.foreach(_.updateCoordinatesByOffset(offset))
//    adoptRoleIntention.foreach(_.updateCoordinatesByOffset(offset))
//  }
//
//  override def normalizeCoordinates(maxWidth: Option[Int], maxHeight: Option[Int]): Unit = {
//    currentTravelIntention.foreach(_.normalizeCoordinates(maxWidth, maxHeight))
//    detachBlocksIntention.foreach(_.normalizeCoordinates(maxWidth, maxHeight))
//    adoptRoleIntention.foreach(_.normalizeCoordinates(maxWidth, maxHeight))
//  }
//
//  override def explain(): String = {
//    currentTravelIntention.map(t => s"exploring to ${t.target} ${t.explain()}").getOrElse("exploring to unknown")
//  }
}
