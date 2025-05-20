package intentions

import action.SkipAction
import model.{AgentAction, Coordinate, Observation}

class IdleIntention extends Intention {

  /** Returns the priority of the intention. Lower means lower importance. */
  def getPriority: Double = 10.0

  /** Always returns a SkipAction since the intention is to do nothing. */
  def planNextAction(observation: Observation): AgentAction = {
    SkipAction()
  }

  /** This intention never finishes. */
  def checkFinished(observation: Observation): Boolean = false

  /** No coordinate offset is needed for IdleIntention. */
  def updateCoordinatesByOffset(offset: Coordinate): Unit = {}

  /** No normalization needed for IdleIntention. */
  def normalizeCoordinates(): Unit = {}

  /** Returns a simple explanation. */
  def explain: String = "idling"
}
