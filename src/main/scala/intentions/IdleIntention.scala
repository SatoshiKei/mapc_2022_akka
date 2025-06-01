package intentions

import action.SkipAction
import model.{AgentAction, Coordinate, Observation}

class IdleIntention extends Intention {

  def getPriority: Double = 10.0

  def planNextAction(observation: Observation): AgentAction = {
    SkipAction()
  }

  def checkFinished(observation: Observation): Boolean = false

  def explain(): String = "idling"
}
