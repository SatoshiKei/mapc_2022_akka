package action

import model.AgentAction

object MoveAction {
  def apply(direction: String): AgentAction = {
    require(Set("n", "s", "e", "w").contains(direction), s"Invalid move direction: $direction")
    AgentAction("move", Seq(direction))
  }
}
