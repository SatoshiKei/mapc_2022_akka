package intentions

import action.{ConnectAction, SkipAction}
import model.{AgentAction, Coordinate, Observation, Task}

class ConnectBlocksIntention(task: Task, goalZone: Coordinate) extends Intention {
  private var connectionIssued = false

  override def explain(): String = "connecting blocks at goal zone"

  override def planNextAction(observation: Observation): AgentAction = {
    if (connectionIssued) return SkipAction()

    val assembly = observation.taskStatus(task.name).getAssembly(observation.agentId).get
    val isRecipient = assembly.recipient == observation.agentId

    val attachedBlocks = observation.attached.map(_ + observation.currentPos)
    val neighbors = observation.knownAgents.filter {
      case (id, known) =>
        id != observation.agentId &&
          observation.taskStatus(task.name).getAssembly(id).exists(remote => observation.translateRemoteCoordinate(remote.goalZone).get == goalZone) //TODO - NONE
    }

    // Recipient waits
    if (isRecipient) {
      return SkipAction()
    }

    // Try to connect with recipient or others
    neighbors.foreach { case (partner, known) =>
      val partnerPos = known.offset
      val partnerBlock = findLastBlockOfAgent(partnerPos, observation)
      val myBlock = findLastBlockOfAgent(observation.currentPos, observation)

      val relative = myBlock.toRelative(partnerBlock)

      if (relative.isAdjacentTo(Coordinate(0, 0))) {
        connectionIssued = true
        return ConnectAction(partner, relative)
      } else {
        val moveTarget = partnerBlock + relative * -1
        return new TravelIntention(moveTarget).planNextAction(observation)
      }
    }

    SkipAction()
  }

  private def findLastBlockOfAgent(base: Coordinate, obs: Observation): Coordinate = {
    obs.attached.map(_ + base).maxBy(b => b.distanceTo(goalZone))
  }

  override def checkFinished(observation: Observation): Boolean = observation.allRequirementsMet(task)
}
