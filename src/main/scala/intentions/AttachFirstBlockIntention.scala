package intentions

import action.{AttachAction, ClearAction, DetachAction, RequestAction, SkipAction}
import model._

import scala.util.Random

class AttachFirstBlockIntention(blockType: String, position: Coordinate) extends Intention {

  private var targetCoord: Option[Coordinate] = None
  private var travelIntention: Option[TravelIntention] = None
  private var roleIntention: Option[AdoptRoleIntention] = None
  private var exploreIntention: Option[ExploreIntention] = None
  private var requiredRole: Option[String] = None
  private var requiredAction: Option[String] = None
  private var finished: Boolean = false

  override def explain(): String = {
    "attaching first block of type " + blockType
  }

  override def planNextAction(observation: Observation): AgentAction = {

    //TODO - Check if range 1 attachment
    if (observation.attached.nonEmpty && !observation.isBlockAttached(blockType)) {
      DetachAction(Coordinate.toDirection(observation.attached.head).get)
    }

    if (observation.isBlockAttached(blockType)) {
      println(observation.agentId + " is trying to complete a task that has already been completed, causing it to hault")
      return SkipAction()
    }

    // Step 1: Determine target block coordinate
    if (targetCoord.isEmpty) {
      val abandoned = findAbandonedBlock(observation)
      val dispenser = observation.findClosestDispenser(blockType)

      val currentRole = observation.currentRole
      val attachRoles = observation.simulation.getRolesWithAction("attach")
      val requestRoles = observation.simulation.getRolesWithAction("request")

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
            requiredAction = Some("attach")
          } else {
            println(observation.agentId + "is going after block of type " + blockType + " from dispenser at " + dispenser)
            targetCoord = Some(dCoord)
            requiredRole = dRole
            requiredAction = Some("request")
          }
        case (Some((aCoord, aRole, _)), None) =>
          println(observation.agentId + "is going after block of type " + blockType + " abandoned at " + abandoned)
          targetCoord = Some(aCoord)
          requiredRole = aRole
          requiredAction = Some("attach")
        case (None, Some((dCoord, dRole, _))) =>
          println(observation.agentId + "is going after block of type " + blockType + " from dispenser at " + dispenser)
          targetCoord = Some(dCoord)
          requiredRole = dRole
          requiredAction = Some("request")
        case _ =>
          println(observation.agentId + "could not find any blocks of type " + blockType)
          if (exploreIntention.isEmpty) exploreIntention = Some(new ExploreIntention())
          return exploreIntention.get.planNextAction(observation)
      }

    }

    val target = targetCoord.get
    val isAdjacent = observation.currentPos.neighbors(includeDiagonals = false).contains(target)

    // Step 2: Check if role needs to be adopted BEFORE moving
    if (requiredAction.exists(action => !observation.simulation.getRolesWithAction(action).contains(observation.currentRole.getOrElse("default")))) {
      if (roleIntention.isEmpty)
        roleIntention = Some(new AdoptRoleIntention(requiredRole.get))
      return roleIntention.get.planNextAction(observation)
    }

    // Step 3: Move adjacent
    if (!isAdjacent) {
      val adjacentCoordinate = target.mostFavorableNeighbor(observation.globalMap)
      println(observation.agentId + " is going to coordinate " + adjacentCoordinate.get + " adjacent to block/dispenser at" + target + "and can attach: " + hasAttachRole(observation) + " and can request: " + hasRequestRole(observation))
      if (travelIntention.isEmpty || travelIntention.get.target != adjacentCoordinate.get) {
        travelIntention = Some(new TravelIntention(adjacentCoordinate.get))
      }
      return travelIntention.get.planNextAction(observation)
    }

    // Step 4: At target — check type of entity
    var value = observation.getTileType(target).getOrElse("empty")

    val dispenserRelative = target - observation.currentPos
    if (observation.things.count(t => t.position == dispenserRelative) > 1) {
      value = "block"
      println(observation.agentId + "'s has requested a block and should now attached it")
    }

    value match {
      case "dispenser" =>
        println(observation.agentId + "at " + observation.currentPos +  " is requesting a block for a " + observation.currentPos.toDirection(target).getOrElse("N/A") + " at " + target)
        RequestAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "block" =>
        AttachAction(observation.currentPos.toDirection(target).getOrElse("n"))

      case "empty" =>
        println("Target: " + target )
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
          observation.globalMap.get(n).exists(v => Set("agent", "marker").contains(v.`type`.toLowerCase))
        )
        if (isIsolated) Some(abs) else None
    }.flatten.headOption
  }

  def costToAcquire(observation: Observation, roleNeeded: Option[String]): Int = {
    if (roleNeeded.isEmpty || observation.currentRole.exists(roleNeeded.contains)) 0
    else math.round(
      observation.getKnownRoleZones
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

