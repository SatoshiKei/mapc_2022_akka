package intentions

import model.{AgentAction, Coordinate, Observation}

/**
 * Clears all clearable cells in an area.
 */
class ClearAreaIntention(val center: Coordinate, val radius: Int = 5) extends Intention {

  private var currentUnitIntention: Option[ClearUnitIntention] = None
  private val skipIntention = new SkipIntention()

  override def planNextAction(observation: Observation): AgentAction = {
    val clearableCoords = getClearableCoords(observation)

    if (clearableCoords.isEmpty) {
      return skipIntention.planNextAction(observation)
    }

    val closest = clearableCoords.minBy(_.manhattanDistance(observation.currentPos))

    if (currentUnitIntention.isEmpty || currentUnitIntention.get.target != closest) {
      currentUnitIntention = Some(new ClearUnitIntention(closest))
    }

    val lowEnergy = observation.simulation.getClearEnergyCost * 2.4 > observation.energy
    if (lowEnergy && closest.manhattanDistance(observation.currentPos) == 1) {
      return skipIntention.planNextAction(observation)
    }

    currentUnitIntention.get.planNextAction(observation)
  }

  override def checkFinished(observation: Observation): Boolean = getClearableCoords(observation).isEmpty

  private def getClearableCoords(observation: Observation): List[Coordinate] = {
    center.neighbors(range = radius).filter { coord =>
      val mapValue = observation.globalMap.get(coord)
      val attachedCoords = observation.attached.map(c => observation.currentPos + c)
      mapValue.exists(v => v == "block" || v == "obstacle" || v == "unknown") && !attachedCoords.contains(coord)
    }
  }

}