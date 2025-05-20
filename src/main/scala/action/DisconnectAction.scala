package action

import model.{AgentAction, Coordinate}

object DisconnectAction {
  def apply(first: Coordinate, second: Coordinate): AgentAction =
    new AgentAction("disconnect", Seq(
      first.x.toString, first.y.toString,
      second.x.toString, second.y.toString
    ))
}
