package action

import model.AgentAction

object SubmitAction {
  def apply(taskName: String): AgentAction =
    new AgentAction("submit", Seq(taskName))
}

