package action

import model.AgentAction

object RequestAction {
  def apply(direction: String): AgentAction = new AgentAction("request", Seq(direction))
}
