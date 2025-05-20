package action

import model.AgentAction

object SkipAction {
  def apply(): AgentAction = AgentAction("skip")
}
