package intentions

import action.{AttachAction, SkipAction, SubmitAction}
import model._

import scala.collection.mutable

class CompleteTaskIntention(task: Task, goalZone: Coordinate) extends ScoredIntention {

  private var blockPlan: List[(Coordinate, String)] = task.requirements.map(r => (Coordinate(r.coordinate.x, r.coordinate.y), r.blockType)).toList
  private var subIntention: Option[Intention] = None

//  override def score(observation: Observation): Double = {
//    if (task.deadline <= observation.simulation.getSimulationStep) 0.0
//    else 1.0 + task.reward - observation.currentPos.distanceTo(Coordinate(0,0)) //TO DO task.goal
//  }

//  override def score(observation: Observation): Double = {
//    if (task.deadline <= observation.simulation.getSimulationStep) 0.0
//    else {
//      val goalDistance = observation.currentPos.distanceTo(goalZone)
//      task.reward - goalDistance
//    }
//  }

  override def score(observation: Observation): Double = {
    val step = observation.simulation.getSimulationStep

    // If the task is expired, it’s not worth pursuing
    if (task.deadline <= step) return 0.0

    // Penalize long travel distances and reward higher rewards
    val distancePenalty = observation.currentPos.distanceTo(goalZone).toDouble
    val rewardScore = task.reward.toDouble

    // How close are we to the deadline?
    val timeLeft = (task.deadline - step).toDouble
    val urgencyBoost = if (timeLeft < 20) 1.5 else 1.0

    // Bonus if the agent already has at least one correct block
    val satisfiedCount = task.requirements.count { case req =>
      observation.attached.exists { coord =>
        coord == Coordinate(req.coordinate.x, req.coordinate.y) &&
          observation.globalMap.get(coord).contains(req.blockType)
      }
    }
    val progressBonus = satisfiedCount * 10.0

    // Total score combines reward, urgency, and progress, minus the travel cost
    urgencyBoost * (rewardScore + progressBonus) - distancePenalty
  }


  //  override def planNextAction(observation: Observation): AgentAction = {
//    // Filter out already satisfied blocks
//    val remaining = blockPlan.filterNot { case (relCoord, blockType) =>
//      observation.attached.exists { attachedCoord =>
//        attachedCoord == relCoord && observation.globalMap.get(attachedCoord).contains(blockType)
//      }
//    }
//
//    // If done, skip
//    if (remaining.isEmpty) {
//      return SkipAction()
//    }
//
//    // Pick the next block to attach
//    val (targetRelCoord, targetBlockType) = remaining.head
//
//    // If agent doesn’t already have that block, initiate BlockCollectionIntention
//    if (!observation.attached.exists(c => observation.globalMap.get(c).contains(targetBlockType))) {
//      if (!subIntention.exists(_.isInstanceOf[AttachFirstBlockIntention])) {
//        subIntention = Some(new AttachFirstBlockIntention(targetBlockType))
//      }
//      return subIntention.get.planNextAction(observation)
//    }
//
//    // Agent has the block → Move into position to attach it at correct relCoord
//    val desiredAbsCoord = observation.currentPos + targetRelCoord.rotateToFacing(observation.orientation)
//
//    if (observation.currentPos != desiredAbsCoord) {
//      if (!subIntention.exists(_.isInstanceOf[TravelIntention])) {
//        subIntention = Some(new TravelIntention(desiredAbsCoord))
//      }
//      return subIntention.get.planNextAction(observation)
//    }
//
//    // Ready to attach the block
//    val attachDirection = observation.currentPos.toDirection(desiredAbsCoord).getOrElse("n")
//    blockPlan = blockPlan.tail
//    subIntention = None
//    AttachAction(attachDirection)
//  }

  override def planNextAction(observation: Observation): AgentAction = {
    // Step 0: Check if the correct role is adopted
    val maybeReservedRole = observation.simulation.getReservedRoles().get(observation.agentId).map(_.name)
    val currentRole = observation.currentRole

    val roleMismatch = maybeReservedRole.exists(reserved => !currentRole.contains(reserved))
    if (roleMismatch) {
      if (!subIntention.exists(_.isInstanceOf[AdoptRoleIntention])) {
        subIntention = Some(new AdoptRoleIntention(maybeReservedRole.get))
      }
      return subIntention.get.planNextAction(observation)
    }

    // Step 1: Filter out already satisfied blocks
    val remaining = blockPlan.filterNot { case (relCoord, blockType) =>
      observation.attached.exists { attachedCoord =>
        attachedCoord == relCoord && observation.globalMap.get(attachedCoord).contains(blockType)
      }
    }

    // Step 2: If done, submit
    if (remaining.isEmpty) {
      // Step 2a: Go to goal zone
      if (observation.currentPos != goalZone) {
        if (!subIntention.exists(_.isInstanceOf[TravelIntention])) {
          subIntention = Some(new TravelIntention(goalZone))
        }
        return subIntention.get.planNextAction(observation)
      }

      // Step 2b: Check role again before submitting
      val submitRoleMismatch = maybeReservedRole.exists(reserved => !currentRole.contains(reserved))
      if (submitRoleMismatch) {
        if (!subIntention.exists(_.isInstanceOf[AdoptRoleIntention])) {
          subIntention = Some(new AdoptRoleIntention(maybeReservedRole.get))
        }
        return subIntention.get.planNextAction(observation)
      }

      // Step 2c: Submit task
      subIntention = None
      return SubmitAction(task.name)
    }

    // Step 3: Pick next block
    val (targetRelCoord, targetBlockType) = remaining.head

    // Step 4: Collect block if not yet attached
    if (!observation.attached.exists(c => observation.globalMap.get(c).contains(targetBlockType))) {
      if (!subIntention.exists(_.isInstanceOf[AttachFirstBlockIntention])) {
        subIntention = Some(new AttachFirstBlockIntention(targetBlockType))
      }
      return subIntention.get.planNextAction(observation)
    }

    // Step 5: Move into position to attach it at correct relCoord
    val desiredAbsCoord = observation.currentPos + targetRelCoord.rotateToFacing(observation.orientation)
    if (observation.currentPos != desiredAbsCoord) {
      if (!subIntention.exists(_.isInstanceOf[TravelIntention])) {
        subIntention = Some(new TravelIntention(desiredAbsCoord))
      }
      return subIntention.get.planNextAction(observation)
    }

    // Step 6: Attach the block
    val attachDirection = targetRelCoord.rotateToFacing(observation.orientation).toDirection.getOrElse("n")
    blockPlan = blockPlan.tail
    subIntention = None
    return AttachAction(attachDirection)
  }



//    override def planNextAction(observation: Observation): AgentAction = {
//    // Step 0: Check if the correct role is adopted
//    val maybeReservedRole = observation.simulation.getReservedRoles().get(observation.agentId).map(_.name)
//    val currentRole = observation.currentRole
//
//    val roleMismatch = maybeReservedRole.exists(reserved => !currentRole.contains(reserved))
//
//    if (roleMismatch) {
//      // Insert AdoptRoleIntention if not already trying to adopt
//      if (!subIntention.exists(_.isInstanceOf[AdoptRoleIntention])) {
//        subIntention = Some(new AdoptRoleIntention(maybeReservedRole.get))
//      }
//      return subIntention.get.planNextAction(observation)
//    }
//
//    // Step 1: Filter out already satisfied blocks
//    val remaining = blockPlan.filterNot { case (relCoord, blockType) =>
//      observation.attached.exists { attachedCoord =>
//        attachedCoord == relCoord && observation.globalMap.get(attachedCoord).contains(blockType)
//      }
//    }
//
//    // Step 2: If done, skip
//    if (remaining.isEmpty) {
//      return SkipAction()
//    }
//
//    // Step 3: Pick next block
//    val (targetRelCoord, targetBlockType) = remaining.head
//
//    // Step 4: Collect block if not yet attached
//    if (!observation.attached.exists(c => observation.globalMap.get(c).contains(targetBlockType))) {
//      if (!subIntention.exists(_.isInstanceOf[AttachFirstBlockIntention])) {
//        subIntention = Some(new AttachFirstBlockIntention(targetBlockType))
//      }
//      return subIntention.get.planNextAction(observation)
//    }
//
//    // Step 5: Move into position to attach it at correct relCoord
//    val desiredAbsCoord = observation.currentPos + targetRelCoord.rotateToFacing(observation.orientation)
//    if (observation.currentPos != desiredAbsCoord) {
//      if (!subIntention.exists(_.isInstanceOf[TravelIntention])) {
//        subIntention = Some(new TravelIntention(desiredAbsCoord))
//      }
//      return subIntention.get.planNextAction(observation)
//    }
//
//    // Step 6: Ready to attach the block
//    val attachDirection = targetRelCoord.rotateToFacing(observation.orientation).toDirection.getOrElse("n")
//
//    blockPlan = blockPlan.tail
//    subIntention = None
//    AttachAction(attachDirection)
//
//    // Step 7: If all blocks are attached, go to goal zone
//    if (blockPlan.isEmpty) {
//      val adjacentToGoal = observation.currentPos.neighbors(includeDiagonals = false).contains(goalZone)
//      if (!adjacentToGoal) {
//        if (!subIntention.exists(_.isInstanceOf[TravelIntention])) {
//          subIntention = Some(new TravelIntention(goalZone))
//        }
//        return subIntention.get.planNextAction(observation)
//      }
//
//      // Step 8: Check role before submitting
//      val maybeReservedRole = observation.simulation.getReservedRoles().get(observation.agentId).map(_.name)
//      val currentRole = observation.currentRole
//      val roleMismatch = maybeReservedRole.exists(reserved => !currentRole.contains(reserved))
//
//      if (roleMismatch) {
//        if (!subIntention.exists(_.isInstanceOf[AdoptRoleIntention])) {
//          subIntention = Some(new AdoptRoleIntention(maybeReservedRole.get))
//        }
//        return subIntention.get.planNextAction(observation)
//      }
//    }
//
//      subIntention = None
//      return SubmitAction(task.name)
//
//  }


  override def checkFinished(observation: Observation): Boolean = {
    blockPlan.isEmpty
  }



}
