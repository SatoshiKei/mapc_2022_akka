package intentions

import action.{AttachAction, ClearAction, RequestAction, SkipAction}
import model._

import scala.util.Random

class AttachFirstBlockIntention(blockType: String) extends Intention {

  private var targetCoord: Option[Coordinate] = None
  private var travelIntention: Option[TravelIntention] = None
  private var roleIntention: Option[AdoptRoleIntention] = None
  private var finished: Boolean = false

  override def explain(): String = {
    "attaching first block"
  }

  override def planNextAction(observation: Observation): AgentAction = {
    if (finished) return SkipAction()

    // Step 1: Determine target block coordinate
    if (targetCoord.isEmpty) {
      val abandoned = findAbandonedBlock(observation)
      val dispenser = observation.findClosestDispenser(blockType)

      (abandoned, dispenser) match {
        case (Some(a), Some(d)) =>
          val from = observation.currentPos
          targetCoord = Some(if (from.manhattanDistance(a) <= from.manhattanDistance(d)) a else d)
        case (Some(a), None) => targetCoord = Some(a)
        case (None, Some(d)) => targetCoord = Some(d)
        case _ => return SkipAction()
      }
    }

    val target = targetCoord.get
    val isAdjacent = observation.currentPos.neighbors(includeDiagonals = false).contains(target)

    // Step 2: Move adjacent
    if (!isAdjacent) {
      if (travelIntention.isEmpty || travelIntention.get.target != target) {
        travelIntention = Some(new TravelIntention(target))
      }
      return travelIntention.get.planNextAction(observation)
    }

    // Step 3: At target — check type of entity
    val value = observation.globalMap.getOrElse(target, "empty")

    value match {
      case "dispenser" =>
        // Ensure we have the right role
        if (!hasAttachRequestRole(observation)) {
          if (roleIntention.isEmpty)
            roleIntention = Some(new AdoptRoleIntention("worker")) // adjust if logic allows better role guess
          return roleIntention.get.planNextAction(observation)
        }
        finished = true
        RequestAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "block" =>
        if (!observation.globalMap.get(target).contains(blockType)) {
          return ClearAction(observation.currentPos.toRelative(target))
        }
        if (!hasAttachRequestRole(observation)) {
          if (roleIntention.isEmpty)
            roleIntention = Some(new AdoptRoleIntention("worker"))
          return roleIntention.get.planNextAction(observation)
        }
        finished = true
        AttachAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "empty" =>
        return SkipAction()

      case _ =>
        return ClearAction(observation.currentPos.toRelative(target))
    }
  }

  override def checkFinished(observation: Observation): Boolean = finished

  private def hasAttachRequestRole(observation: Observation): Boolean = {
    observation.currentRole.contains("worker") || observation.currentRole.contains("constructor")
  }

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

