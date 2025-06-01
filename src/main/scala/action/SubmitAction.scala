package action

import model.AgentAction

object SubmitAction {
  def apply(taskName: String): AgentAction = AgentAction("submit", Seq(taskName))
}

