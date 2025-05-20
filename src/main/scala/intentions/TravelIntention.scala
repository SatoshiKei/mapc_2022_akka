


package intentions
import action.{ClearAction, MoveAction, RotateAction, SkipAction}
import io.circe.Json
import model.{AgentAction, Coordinate, Observation}
import planner.{PathExecutor, Planner}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class TravelIntention(var target: Coordinate, val planner: Planner = new PathExecutor()) extends Intention {

  override def planNextAction(observation: Observation): AgentAction = {
    planner.nextAction(observation, target)
  }

  override def checkFinished(observation: Observation): Boolean =
    observation.currentPos == target

  override def shouldAbort(observation: Observation): Boolean = false

  def explain(): String = s"traveling to $target"
}

