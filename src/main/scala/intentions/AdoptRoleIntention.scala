package intentions

import model.{AgentAction, Observation}

class AdoptRoleIntention(roleName: String) {

  var finished: Boolean = false

  def planNextAction(observation: Observation): AgentAction = {
    observation.currentRole match {
      case Some(current) if current == roleName =>
        finished = true
        AgentAction("skip")
      case _ =>
        AgentAction("adopt", Seq(roleName))
    }
  }


  def checkFinished(observation: Observation): Boolean = {
    finished || observation.hasRole(roleName)
  }

  def updateCoordinatesByOffset(offset: model.Coordinate): Unit = {
    // No coordinates are involved in adopting a role
  }

  def normalizeCoordinates(): Unit = {
    // No normalization needed
  }

  def explain(): String = {
    s"Adopting role: $roleName"
  }
}
