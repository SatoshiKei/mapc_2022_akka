package intentions

import action.{ClearAction, SkipAction}
import model.{AgentAction, Coordinate, Observation}

import scala.collection.mutable

/**
 * Clears a single coordinate (unit obstacle or block).
 */
class ClearUnitIntention(val target: Coordinate) extends Intention {

  override def explain(): String = {
    "clear a single coordinate"
  }

  private var travelIntention: Option[TravelIntention] = None
  private var skipIntention: SkipIntention = new SkipIntention()
  private var finished: Boolean = false

  override def planNextAction(observation: Observation): AgentAction = {
    if (observation.isUnknown(target)) {
      if (travelIntention.isEmpty)
        travelIntention = Some(new TravelIntention(target))
      return travelIntention.get.planNextAction(observation)
    }

    val freeNeighbors = target.neighbors().filter { c =>
      c == observation.currentPos || observation.globalMap.get(c).forall(t => t != "block" && t != "entity")
    }

    if (freeNeighbors.isEmpty) {
      finished = true
      return skipIntention.planNextAction(observation)
    }

    val closest = freeNeighbors.minBy(_.manhattanDistance(observation.currentPos))

    if (travelIntention.isEmpty || travelIntention.exists(_.target != closest))
      travelIntention = Some(new TravelIntention(closest))

    if (!travelIntention.get.checkFinished(observation)) {
      travelIntention.get.planNextAction(observation)
    } else {
      ClearAction(observation.currentPos.toRelative(target))
    }
  }

  override def checkFinished(observation: Observation): Boolean = {
    finished || observation.globalMap.get(target).exists(t => t == "cleared" || t == "empty" || t == "dispenser")
  }

}
