package intentions

import action.{AttachAction, ClearAction, RequestAction, SkipAction}
import model._

import scala.util.Random

class AttachFirstBlockIntention(blockType: String) extends Intention {

  private var targetCoord: Option[Coordinate] = None
  private var travelIntention: Option[TravelIntention] = None
  private var roleIntention: Option[AdoptRoleIntention] = None
  private var exploreIntention: Option[ExploreIntention] = None
  private var finished: Boolean = false

  override def explain(): String = {
    "attaching first block of type " + blockType
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
        case (Some(a), None) => {
          println(observation.agentId + "is going after block of type " + blockType + " abandoned at " + a)
          targetCoord = Some(a)
        }
        case (None, Some(d)) => {
          println(observation.agentId + "is going after block of type " + blockType + " from dispenser at " + d)
          targetCoord = Some(d)
        }
        case _ => {
          println(observation.agentId + "could not find any blocks of type " + blockType)
          if (exploreIntention.isEmpty) {
            exploreIntention = Some(new ExploreIntention())
          }
          return exploreIntention.get.planNextAction(observation)
        }
      }
    }

    val target = targetCoord.get
    val isAdjacent = observation.currentPos.neighbors(includeDiagonals = false).contains(target)

    // Step 2: Move adjacent
    if (!isAdjacent) {
      println(observation.agentId + " is going to coordinate " + target + " adjacent to block/dispenser at" + targetCoord.get + "and can attach: " + hasAttachRole(observation) + " and can request: " + hasRequestRole(observation))
      if (travelIntention.isEmpty || travelIntention.get.target != target) {
        travelIntention = Some(new TravelIntention(target))
      }
      return travelIntention.get.planNextAction(observation)
    }

    //TODO - Make sure Agent is facing dispenser

    // Step 3: At target — check type of entity
    val value = observation.globalMap.getOrElse(target, "empty")

    value match {
      case "dispenser" =>
        // Ensure we have the right role
        if (!hasRequestRole(observation)) {
          if (roleIntention.isEmpty)
            roleIntention = Some(new AdoptRoleIntention(observation.simulation.getRolesWithAction("request").headOption.get)) //TODO - ensure this can't be null
          return roleIntention.get.planNextAction(observation)
        }
        finished = true
        RequestAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "block" =>
        if (!observation.globalMap.get(target).contains(blockType)) {
          return ClearAction(observation.currentPos.toRelative(target))
        }
        if (!hasAttachRole(observation)) {
          if (roleIntention.isEmpty)
            roleIntention = Some(new AdoptRoleIntention(observation.simulation.getRolesWithAction("attach").headOption.get)) //TODO - ensure this can't be null
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

  private def hasAttachRole(observation: Observation): Boolean = {
    val attachCapableRoles = observation.simulation.getRolesWithAction("attach")
    observation.currentRole.exists(attachCapableRoles.contains)
  }

  private def hasRequestRole(observation: Observation): Boolean = {
    val attachCapableRoles = observation.simulation.getRolesWithAction("request")
    observation.currentRole.exists(attachCapableRoles.contains)
  }

  private def findAbandonedBlock(observation: Observation): Option[Coordinate] = {
    observation.things.collect {
      case t if t.`type` == "block" && t.details == blockType =>
        val abs = observation.currentPos + Coordinate(t.x, t.y) //Deprecated rotate to facing
        val neighbors = abs.neighbors(includeDiagonals = false)
        val isIsolated = !neighbors.exists(n =>
          observation.globalMap.get(n).exists(v => Set("agent", "block", "marker").contains(v.toLowerCase))
        )
        if (isIsolated) Some(abs) else None
    }.flatten.headOption
  }
}

