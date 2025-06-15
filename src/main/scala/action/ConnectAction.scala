package action

import model.{AgentAction, Coordinate}

object ConnectAction {
  def apply(target: String, pos: Coordinate): AgentAction =
    new AgentAction("connect", Seq(target, pos.x.toString, pos.y.toString))
}
