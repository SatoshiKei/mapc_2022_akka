package planner

import model.{AgentAction, Coordinate, Observation}

trait Planner {
  def nextAction(observation: Observation, target: Coordinate): AgentAction
}
