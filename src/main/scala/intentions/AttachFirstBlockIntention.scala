package intentions

import action.{AttachAction, ClearAction, RequestAction, SkipAction}
import model._

import scala.util.Random

class AttachFirstBlockIntention(blockType: String) extends Intention {

  private var targetCoord: Option[Coordinate] = None
  private var travelIntention: Option[TravelIntention] = None
  private var finished: Boolean = false

  override def planNextAction(observation: Observation): AgentAction = {
    if (finished) return SkipAction()

    // Step 1: Determine target block coordinate if not yet selected
    if (targetCoord.isEmpty) {
      val abandoned = findAbandonedBlock(observation)
      val dispenser = observation.findClosestDispenser(blockType)

      (abandoned, dispenser) match {
        case (Some(a), Some(d)) =>
          val from = observation.currentPos
          targetCoord = Some(if (from.manhattanDistance(a) <= from.manhattanDistance(d)) a else d)
        case (Some(a), None) =>
          targetCoord = Some(a)
        case (None, Some(d)) =>
          targetCoord = Some(d)
        case _ =>
          return SkipAction()
      }
    }

    val target = targetCoord.get
    val adjacent = observation.currentPos.neighbors(includeDiagonals = false).contains(target)

    // Step 2: If not adjacent, travel
    if (!adjacent) {
      if (travelIntention.isEmpty || travelIntention.get.target != target) {
        travelIntention = Some(new TravelIntention(target))
      }
      return travelIntention.get.planNextAction(observation)
    }

    // Step 3: Now we are adjacent → decide what to do
    val value = observation.globalMap.getOrElse(target, "empty")

    value match {
      case "dispenser" =>
        finished = true
        RequestAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "block" =>
        // If it's the wrong type, clear it
        if (!observation.globalMap.get(target).contains(blockType)) {
          return ClearAction(observation.currentPos.toRelative(target))
        }
        // Otherwise, attach it
        finished = true
        AttachAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "empty" =>
        // Wait or retry later if nothing there
        return SkipAction()

      case _ =>
        // If blocked by another entity or obstacle
        return ClearAction(observation.currentPos.toRelative(target))
    }
  }

  override def checkFinished(observation: Observation): Boolean = finished

  private def findAbandonedBlock(observation: Observation): Option[Coordinate] = {
    observation.things.collect {
      case t if t.`type` == "block" && t.details == blockType =>
        val abs = observation.currentPos + Coordinate(t.x, t.y).rotateToFacing(observation.orientation)
        val neighbors = abs.neighbors(includeDiagonals = false)
        val isIsolated = !neighbors.exists(n =>
          observation.globalMap.get(n).exists(v => Set("agent", "block", "marker").contains(v.toLowerCase))
        )
        if (isIsolated) Some(abs) else None
    }.flatten.headOption
  }
}
