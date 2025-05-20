package action

import model.AgentAction

object RotateAction {
  def apply(direction: String): AgentAction = {
    require(direction == "cw" || direction == "ccw", "Direction must be 'cw' or 'ccw'")
    AgentAction("rotate", Seq(direction))
  }
}
