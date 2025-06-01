package intentions

import action.{AttachAction, SkipAction, SubmitAction}
import model._

import scala.collection.mutable

class CompleteTaskIntention(task: Task, goalZone: Coordinate) extends ScoredIntention {

  private var blockPlan: List[(Coordinate, String)] = task.requirements.map(r => (Coordinate(r.coordinate.x, r.coordinate.y), r.`type`)).toList
  private var subIntention: Option[Intention] = None

  override def explain(): String = {
    val explainSubIntention = subIntention.map(_.getClass.getSimpleName).getOrElse("stuff")
    "completing " + task.name + " by doing " + explainSubIntention
  }

  override def score(obs: Observation): Double = {
    if (task.deadline <= obs.simulation.getSimulationStep) return 0.0
    if (obs.getKnownGoalZones.isEmpty) return 0.0

    val progress = task.requirements.count { r =>
      obs.attached.exists(c => obs.globalMap.get(c).exists(thing => thing.`type` == r.`type`))
    }

    val isNearGoal = obs.getKnownGoalZones.exists(_.distanceTo(obs.currentPos) <= 5)
    val rewardWeight = 2.0
    val progressWeight = 15.0
    val goalProximityBonus = if (isNearGoal) 10 else 0

    rewardWeight * task.reward + progressWeight * progress - obs.currentPos.distanceTo(goalZone) + goalProximityBonus
  }

  override def planNextAction(observation: Observation): AgentAction = {
    if (checkFinished(observation)) return SkipAction()

    // Step 1: Find the first attachable block from the task plan (adjacent-only)
    val immediateRequirement = task.requirements.find { req =>
      val rel = req.coordinate
      Set(Coordinate(0, 1), Coordinate(0, -1), Coordinate(1, 0), Coordinate(-1, 0)).contains(rel)
    }

    // Step 2: If no adjacent block found, fallback to Explore for now
    if (immediateRequirement.isEmpty) {
      println(s"${observation.agentId} found no immediate attachable block for ${task.name}")
      if (!subIntention.exists(_.isInstanceOf[ExploreIntention])) subIntention = Some(new ExploreIntention())
      return subIntention.get.planNextAction(observation)
    }

    // Step 3: Delegate to AttachFirstBlockIntention for the correct block type
    val blockType = immediateRequirement.get.`type`
    if (subIntention.isEmpty || !subIntention.get.isInstanceOf[AttachFirstBlockIntention]) {
      subIntention = Some(new AttachFirstBlockIntention(blockType))
    }
    subIntention.get.planNextAction(observation)
  }


  //  override def planNextAction(observation: Observation): AgentAction = {
//    // Step 0: Check if the correct role is adopted
//    val maybeReservedRole = observation.simulation.getReservedRoles().get(observation.agentId).map(_.name)
//    val currentRole = observation.currentRole
//
//    val roleMismatch = maybeReservedRole.exists(reserved => !currentRole.contains(reserved))
//    if (roleMismatch) {
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
//    // Step 2: If done, submit
//    if (remaining.isEmpty) {
//      // Step 2a: Go to goal zone
//      if (observation.currentPos != goalZone) {
//        if (!subIntention.exists(_.isInstanceOf[TravelIntention])) {
//          subIntention = Some(new TravelIntention(goalZone))
//        }
//        return subIntention.get.planNextAction(observation)
//      }
//
//      // Step 2b: Check role again before submitting
//      val submitRoleMismatch = maybeReservedRole.exists(reserved => !currentRole.contains(reserved))
//      if (submitRoleMismatch) {
//        if (!subIntention.exists(_.isInstanceOf[AdoptRoleIntention])) {
//          subIntention = Some(new AdoptRoleIntention(maybeReservedRole.get))
//        }
//        return subIntention.get.planNextAction(observation)
//      }
//
//      // Step 2c: Submit task
//      subIntention = None
//      return SubmitAction(task.name)
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
//    val desiredAbsCoord = observation.currentPos + targetRelCoord //Deprecated rotate to facing
//    if (observation.currentPos != desiredAbsCoord) {
//      if (!subIntention.exists(_.isInstanceOf[TravelIntention])) {
//        subIntention = Some(new TravelIntention(desiredAbsCoord))
//      }
//      return subIntention.get.planNextAction(observation)
//    }
//
//    // Step 6: Attach the block
//    val attachDirection = targetRelCoord.toDirection.getOrElse("n") //Deprecated rotate to facing
//    blockPlan = blockPlan.tail
//    subIntention = None
//    AttachAction(attachDirection)
//  }

  override def checkFinished(observation: Observation): Boolean = {
    blockPlan.isEmpty
  }



}
