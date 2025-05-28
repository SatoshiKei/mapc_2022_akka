package intentions

import model._

class AdoptRoleIntention(roleName: String) extends ScoredIntention {

  private var travel: Option[TravelIntention] = None

  override def explain(): String = {
    if (travel.isEmpty) {
      "adopting role " + roleName
    } else {
      "traveling to coordiante " + travel.get.target + " to adopt role " + roleName
    }
  }

  override def score(obs: Observation): Double = {
    if (obs.currentRole.contains(roleName)) 0.0
    else 0.7
  }

  override def planNextAction(observation: Observation): AgentAction = {
    // TODO - This never happens
    if (observation.currentRole.contains(roleName)) {
      println(observation.agentId + " already has role " + roleName)
      return AgentAction("skip")
    }

    //TODO - THIS SHOULD ME REMOVED - DONE ALREADY ON INTENTION HANDLER
    if (observation.getKnownRoleZones.isEmpty) {
      return new ExploreIntention().planNextAction(observation)
    }
    //-------------------------

    if (travel.isDefined && travel.get.target != observation.currentPos) {
      println(observation.agentId + " in AdoptRoleIntention is moving from" + observation.currentPos + " to " + travel.get.target)
      return travel.get.planNextAction(observation)
    }

    // Step 2: Check if in any roleZone
    val inRoleZone = observation.getKnownRoleZones.find { rz =>
      observation.currentPos == rz
    }

    inRoleZone match {
      case Some(_) =>
        AgentAction("adopt", Seq(roleName))

      case None =>
        // Need to move closer
        val closest = if (observation.getKnownRoleZones.nonEmpty) {
          Some(observation.getKnownRoleZones.minBy(_.distanceTo(observation.currentPos)))
        } else None

        closest match {
          case Some(target) =>
            // TODO - travel.isEmpty || travel.get.target != target
            if (travel.isEmpty || travel.get.target != target) {
              travel = Some(new TravelIntention(target))
            }
            travel.get.planNextAction(observation)

          case None =>
            // No known role zone → skip (or fallback to Explore)
            println(observation.agentId + " is not in a role zone")
            AgentAction("skip")
        }
    }
  }

  override def checkFinished(observation: Observation): Boolean = {
    observation.currentRole.contains(roleName)
  }

//  override def updateCoordinatesByOffset(offset: Coordinate): Unit = {
//    travel.foreach(_.updateCoordinatesByOffset(offset))
//  }
//
//  override def normalizeCoordinates(): Unit = {
//    travel.foreach(_.normalizeCoordinates())
//  }
//
//  override def explain(): String = {
//    s"Adopting role: $roleName"
//  }
}
