package intentions

import model.{AgentAction, Coordinate, Observation}

class DetachBlocksIntention extends ScoredIntention {

  var finished: Boolean = false

  override def score(observation: Observation): Double = {
    // Simple logic: if carrying too many blocks, high urgency
    val max = observation.simulation.getMaxBlockRegulation.getOrElse(3)
    val n = observation.attached.size
    if (n == 0) 0.0
    else if (n > max) 1.0
    else n.toDouble / max
  }

  def planNextAction(observation: Observation): AgentAction = {
    if (observation.things.isEmpty) {
      finished = true
      return AgentAction("skip")
    }

    // Detach the first attached block
    val firstAttached = observation.things.head
    val relCoord = firstAttached.position

    // Figure out direction to detach
    directionFromRelativeCoordinate(relCoord) match {
      case Some(dir) =>
        AgentAction("detach", Seq(dir))
      case None =>
        // Cannot find a direction, skip
        AgentAction("skip")
    }
  }

  def checkFinished(observation: Observation): Boolean = {
    observation.things.isEmpty || finished
  }

  def updateCoordinatesByOffset(offset: Coordinate): Unit = {
    // Normally adjusts internal targets, but DetachBlocks doesn't need it
  }

  def normalizeCoordinates(): Unit = {
    // No normalization needed for DetachBlocks either
  }

  def explain(): String = {
    "Detaching blocks if carrying too many."
  }

  private def directionFromRelativeCoordinate(coord: Coordinate): Option[String] = {
    (coord.x, coord.y) match {
      case (0, -1) => Some("n")
      case (0, 1)  => Some("s")
      case (1, 0)  => Some("e")
      case (-1, 0) => Some("w")
      case _       => None
    }
  }
}
