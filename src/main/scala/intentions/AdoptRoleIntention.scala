package intentions

import model._

class AdoptRoleIntention(roleName: String) extends ScoredIntention {

  private var travel: Option[TravelIntention] = None

  override def score(obs: Observation): Double = {
    if (obs.currentRole.contains(roleName)) 0.0
    else 0.7
  }

  override def planNextAction(observation: Observation): AgentAction = {
    // Step 1: Already adopted?
    if (observation.currentRole.contains(roleName)) {
      println(observation.agentId + " already has role " + roleName)
      return AgentAction("skip")
    }

    if (travel.isDefined && travel.get.target != observation.currentPos) {
      return travel.get.planNextAction(observation)
    }


    // Step 2: Check if adjacent to any roleZone
    val adjacentZoneOpt = observation.getKnownRoleZones.find { rz =>
      observation.currentPos.isAdjacentTo(rz)
    }

    adjacentZoneOpt match {
      case Some(_) =>
        // We're adjacent to a role zone — adopt!
        return AgentAction("adopt", Seq(roleName))

      case None =>
        // Need to move closer
        val closest = if (observation.getKnownRoleZones.nonEmpty) {
          Some(observation.getKnownRoleZones.minBy(_.distanceTo(observation.currentPos)))
        } else None

        closest match {
          case Some(target) =>
            if (travel.isEmpty || travel.get.target != target) {
              travel = Some(new TravelIntention(target))
            }
            return travel.get.planNextAction(observation)

          case None =>
            // No known role zone → skip (or fallback to Explore)
            println("No known role zone")
            return AgentAction("skip")
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
