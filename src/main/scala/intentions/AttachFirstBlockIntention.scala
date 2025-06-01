package intentions

import action.{AttachAction, ClearAction, RequestAction, SkipAction}
import model._

import scala.util.Random

class AttachFirstBlockIntention(blockType: String) extends Intention {

  private var targetCoord: Option[Coordinate] = None
  private var travelIntention: Option[TravelIntention] = None
  private var roleIntention: Option[AdoptRoleIntention] = None
  private var exploreIntention: Option[ExploreIntention] = None
  private var requiredRole: Option[String] = None
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

      val currentRole = observation.currentRole
      val attachRoles = observation.simulation.getRolesWithAction("attach")
      val requestRoles = observation.simulation.getRolesWithAction("request")

//      val abandonedCost: Option[Int] = abandoned.map(c => totalCost(observation, c, attachRoles.find(r => !currentRole.contains(r))))
//      val dispenserCost: Option[Int] = dispenser.map(c => totalCost(observation, c, requestRoles.find(r => !currentRole.contains(r))))

      val abandonedCost = abandoned.map { coord =>
        val role = attachRoles.find(r => !currentRole.contains(r))
        val cost = totalCost(observation, coord, role)
        (coord, role, cost)
      }

      val dispenserCost = dispenser.map { coord =>
        val role = requestRoles.find(r => !currentRole.contains(r))
        val cost = totalCost(observation, coord, role)
        (coord, role, cost)
      }

      (abandonedCost, dispenserCost) match {
        case (Some((aCoord, aRole, aCost)), Some((dCoord, dRole, dCost))) =>
          if (aCost <= dCost) {
            println(observation.agentId + "is going after block of type " + blockType + " abandoned at " + abandoned)
            targetCoord = Some(aCoord)
            requiredRole = aRole
          } else {
            println(observation.agentId + "is going after block of type " + blockType + " from dispenser at " + dispenser)
            targetCoord = Some(dCoord)
            requiredRole = dRole
          }
        case (Some((aCoord, aRole, _)), None) =>
          println(observation.agentId + "is going after block of type " + blockType + " abandoned at " + abandoned)
          targetCoord = Some(aCoord)
          requiredRole = aRole
        case (None, Some((dCoord, dRole, _))) =>
          println(observation.agentId + "is going after block of type " + blockType + " from dispenser at " + dispenser)
          targetCoord = Some(dCoord)
          requiredRole = dRole
        case _ =>
          println(observation.agentId + "could not find any blocks of type " + blockType)
          if (exploreIntention.isEmpty) exploreIntention = Some(new ExploreIntention())
          return exploreIntention.get.planNextAction(observation)
      }


//      (abandoned, dispenser) match {
//        case (Some(a), Some(d)) =>
//          val from = observation.currentPos
//          targetCoord = Some(if (from.manhattanDistance(a) <= from.manhattanDistance(d)) a else d)
//        case (Some(a), None) => {
//          println(observation.agentId + "is going after block of type " + blockType + " abandoned at " + a)
//          targetCoord = Some(a)
//        }
//        case (None, Some(d)) => {
//          println(observation.agentId + "is going after block of type " + blockType + " from dispenser at " + d)
//          targetCoord = Some(d)
//        }
//        case _ => {
//          println(observation.agentId + "could not find any blocks of type " + blockType)
//          if (exploreIntention.isEmpty) {
//            exploreIntention = Some(new ExploreIntention())
//          }
//          return exploreIntention.get.planNextAction(observation)
//        }
//      }


    }

    val target = targetCoord.get
    val isAdjacent = observation.currentPos.neighbors(includeDiagonals = false).contains(target)

    // Step 2: Check if role needs to be adopted BEFORE moving
    if (requiredRole.exists(role => !observation.currentRole.contains(role))) {
      if (roleIntention.isEmpty)
        roleIntention = Some(new AdoptRoleIntention(requiredRole.get))
      return roleIntention.get.planNextAction(observation)
    }

    // Step 3: Move adjacent
    if (!isAdjacent) {
      println(observation.agentId + " is going to coordinate " + target + " adjacent to block/dispenser at" + targetCoord.get + "and can attach: " + hasAttachRole(observation) + " and can request: " + hasRequestRole(observation))
      if (travelIntention.isEmpty || travelIntention.get.target != target) {
        travelIntention = Some(new TravelIntention(target))
      }
      return travelIntention.get.planNextAction(observation)
    }

    //TODO - Make sure Agent is facing dispenser

    // Step 4: At target — check type of entity
    val value = observation.getTileType(target).getOrElse("empty")

    value match {
      case "dispenser" =>
        finished = true
        RequestAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "block" =>
        if (!observation.globalMap.get(target).contains(blockType)) {
          return ClearAction(observation.currentPos.toRelative(target))
        }
        finished = true
        AttachAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "empty" =>
        SkipAction()

      case _ =>
        ClearAction(observation.currentPos.toRelative(target))
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
        val abs = observation.currentPos + Coordinate(t.x, t.y)
        val neighbors = abs.neighbors(includeDiagonals = false)
        val isIsolated = !neighbors.exists(n =>
          observation.globalMap.get(n).exists(v => Set("agent", "marker").contains(v.toLowerCase))
        )
        if (isIsolated) Some(abs) else None
    }.flatten.headOption
  }

  def costToAcquire(observation: Observation, roleNeeded: Option[String]): Int = {
    if (roleNeeded.isEmpty || observation.currentRole.exists(roleNeeded.contains)) 0
    else math.round(
      observation.knownRoleZones
        .map(_.distanceTo(observation.currentPos))
        .reduceOption((a, b) => math.min(a, b))
        .getOrElse(100.0)
    ).toInt
  }



  def totalCost(observation: Observation, coord: Coordinate, roleNeeded: Option[String]): Int = {
    val base = observation.currentPos.manhattanDistance(coord)
    val roleCost = costToAcquire(observation, roleNeeded)
    val speed = roleNeeded
      .flatMap(name => observation.simulation.getAllRoles.find(_.name == name))
      .map(_.getSpeed(observation.attached.size))
      .getOrElse(1)

    math.round((base + roleCost).toDouble / speed).toInt
  }

}

