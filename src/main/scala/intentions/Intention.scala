package intentions

import model.{AgentAction, Coordinate, Observation}

trait Intention {

  def planNextAction(observation: Observation): AgentAction

  def checkFinished(observation: Observation): Boolean

  def shouldAbort(observation: Observation): Boolean = false

  def explain(): String
}
