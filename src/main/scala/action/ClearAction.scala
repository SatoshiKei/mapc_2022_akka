package action

import model.{AgentAction, Coordinate}

object ClearAction {
  def apply(relCoordinate: Coordinate): AgentAction = new AgentAction("clear", Seq(relCoordinate.x.toString, relCoordinate.y.toString))
}
