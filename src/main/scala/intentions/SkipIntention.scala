package intentions

import action.SkipAction
import model.{AgentAction, Observation}

class SkipIntention extends Intention {
  override def planNextAction(observation: Observation): AgentAction = SkipAction()
  override def checkFinished(observation: Observation): Boolean = true
  override def explain(): String = "skipping"
}