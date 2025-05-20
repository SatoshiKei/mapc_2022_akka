package action

import model.AgentAction

object DetachAction {
  def apply(direction: String): AgentAction =
    new AgentAction("detach", Seq(direction))
}
