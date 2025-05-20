package planner

import model.{AgentAction, Coordinate, Observation}

trait ClearPlanner {
  def shouldClear(observation: Observation, direction: String): Option[AgentAction]
}
