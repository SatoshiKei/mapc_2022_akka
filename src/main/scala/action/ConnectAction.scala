package action

import model.{AgentAction, Coordinate}

object ConnectAction {
  def apply(toAgentId: String, relCoord: Coordinate, partnerOffset: Coordinate): AgentAction =
    new AgentAction("connect", Seq(toAgentId, relCoord.x.toString, relCoord.y.toString))
}
