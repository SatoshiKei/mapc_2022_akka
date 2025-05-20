package intentions

import model.{AgentAction, Coordinate, Observation}

trait Intention {
  /**
   * Plans and returns the next action to reach its goal.
   */
  def planNextAction(observation: Observation): AgentAction

  /**
   * Returns if the intention has reached its goal.
   */
  def checkFinished(observation: Observation): Boolean

  def shouldAbort(observation: Observation): Boolean = false

}
