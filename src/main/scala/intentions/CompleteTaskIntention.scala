package intentions

import action.{AttachAction, SkipAction}
import model._

import scala.collection.mutable

class CompleteTaskIntention(task: Task, goalZone: Coordinate) extends ScoredIntention {

  private var blockPlan: List[(Coordinate, String)] = task.requirements.map(r => (Coordinate(r.coordinate.x, r.coordinate.y), r.blockType)).toList
  private var subIntention: Option[Intention] = None

  override def score(observation: Observation): Double = {
    if (task.deadline <= observation.simulation.getSimulationStep) 0.0
    else 1.0 + task.reward - observation.currentPos.distanceTo(Coordinate(0,0)) //TO DO task.goal
  }

  override def planNextAction(observation: Observation): AgentAction = {
    // Filter out already satisfied blocks
    val remaining = blockPlan.filterNot { case (relCoord, blockType) =>
      observation.attached.exists { attachedCoord =>
        attachedCoord == relCoord && observation.globalMap.get(attachedCoord).contains(blockType)
      }
    }

    // If done, skip
    if (remaining.isEmpty) {
      return SkipAction()
    }

    // Pick the next block to attach
    val (targetRelCoord, targetBlockType) = remaining.head

    // If agent doesn’t already have that block, initiate BlockCollectionIntention
    if (!observation.attached.exists(c => observation.globalMap.get(c).contains(targetBlockType))) {
      if (!subIntention.exists(_.isInstanceOf[AttachFirstBlockIntention])) {
        subIntention = Some(new AttachFirstBlockIntention(targetBlockType))
      }
      return subIntention.get.planNextAction(observation)
    }

    // Agent has the block → Move into position to attach it at correct relCoord
    val desiredAbsCoord = observation.currentPos + targetRelCoord.rotateToFacing(observation.orientation)

    if (observation.currentPos != desiredAbsCoord) {
      if (!subIntention.exists(_.isInstanceOf[TravelIntention])) {
        subIntention = Some(new TravelIntention(desiredAbsCoord))
      }
      return subIntention.get.planNextAction(observation)
    }

    // Ready to attach the block
    val attachDirection = observation.currentPos.toDirection(desiredAbsCoord).getOrElse("n")
    blockPlan = blockPlan.tail
    subIntention = None
    AttachAction(attachDirection)
  }

  override def checkFinished(observation: Observation): Boolean = {
    blockPlan.isEmpty
  }



}
