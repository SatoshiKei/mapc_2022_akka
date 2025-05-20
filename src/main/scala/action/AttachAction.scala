package action

import model.{AgentAction, Coordinate}

object AttachAction {
  def apply(direction: String): AgentAction = new AgentAction("attach", Seq(direction))
}
