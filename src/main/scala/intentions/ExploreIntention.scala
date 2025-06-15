package intentions

import model._

class ExploreIntention() extends Intention with ScoredIntention {

  private var currentTravelIntention: Option[TravelIntention] = None
  private var detachBlocksIntention: Option[DetachBlocksIntention] = None
  private var adoptRoleIntention: Option[AdoptRoleIntention] = None
  private var relocating: Boolean = false
  private var estimatedMapSize = 100

  override def explain(): String = {
    if (currentTravelIntention.isEmpty)
      "deciding exploration coordinate"
    else
      "exploring " + currentTravelIntention.get.target + " coordinate"
  }

  override def score(obs: Observation): Double =
    if (obs.mapIsFullyExplored) 0.0
    else 1.0 - (obs.globalMap.size / estimatedMapSize)

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
}
