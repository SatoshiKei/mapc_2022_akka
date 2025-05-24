package actors

import action.SkipAction
import intentions._
import model._

import scala.collection.mutable.PriorityQueue

class IntentionHandler() {

//  private val intentions = PriorityQueue.empty[ScoredIntention](Ordering.by(-_.score))

  private var currentIntention: Option[ScoredIntention] = None
  private val fallbackIntentions: Seq[ScoredIntention] = Seq(new ExploreIntention())
  var hysteresis = 0.5

  // Insert base intentions like Explore and Idle
//  def initialize(): Unit = {
//    insertIntention(new ExploreIntention())
//    insertIntention(new IdleIntention())
//  }

  private def evaluateAndMaybeSwitch(observation: Observation): Unit = {
    val candidates = fallbackIntentions ++ discoverNewIntentions(observation)
    val best = candidates.maxBy(_.score(observation))

    // Step 0: Check if this agent is violating any role-based norm
    if (violatesRoleNorm(observation)) {
      currentIntention = Some(new DetachBlocksIntention()) // or new ExploreIntention() as fallback
      return
    }

    val currentScore = currentIntention.map(_.score(observation)).getOrElse(Double.MinValue)
    if (currentIntention.isEmpty || currentIntention.get.checkFinished(observation) ||
      best.score(observation) > currentScore + hysteresis) {
      currentIntention = Some(best)
    }
  }

  def evaluateIntentions(observation: Observation): Unit = {
    val scoredIntents = fallbackIntentions ++ getTaskIntentionIfVisible(observation)
    val best = scoredIntents.maxBy(_.score(observation))

    if (currentIntention.forall(_.checkFinished(observation)) ||
      currentIntention.exists(_.score(observation) < best.score(observation))) {
      currentIntention = Some(best)
    }
  }

//  def reevaluateIntentions(observation: Observation): Unit = {
//    val scored = intentions.map { case ScoredIntention(intention, _) =>
//      ScoredIntention(intention, intention.score(observation))
//    }.toSeq
//
//    currentIntention match {
//      case Some(ci) if !ci.checkFinished(observation) =>
//        val ciPriority = ci.asInstanceOf[ScoredIntention].score(observation)
//        val highest = scored.maxBy(_.priority)
//
//        if (highest.priority > ciPriority + hysteresis) {
//          currentIntention = Some(highest.intention)
//        }
//      case _ =>
//        val highest = scored.maxBy(_.priority)
//        currentIntention = Some(highest.intention)
//    }
//  }



//  def planNextAction(observation: Observation): AgentAction = {
//    val intention: Intention = getCurrentIntention(observation).getOrElse(new IdleIntention())
//    intention.planNextAction(observation)
//  }

  def planNextAction(observation: Observation): AgentAction = {
    evaluateAndMaybeSwitch(observation)
    currentIntention.map(_.planNextAction(observation)).getOrElse(SkipAction())
  }

  private def getTaskIntentionIfVisible(observation: Observation): Seq[ScoredIntention] = {
    observation.simulation.getTasks.map(t => new CompleteTaskIntention(t, Coordinate(0,0)))
  }

  private def discoverNewIntentions(observation: Observation): Seq[ScoredIntention] = {
    val knownGoalZones = observation.goalZones

    val normIntentions = observation.simulation.getActiveNormRequirements("role").flatMap { req =>
      val current = observation.currentRole.getOrElse("default")
      if (current == req.name) {
        Some(new AdoptRoleIntention("default")) // fallback role to avoid violation
      } else None
    } ++ observation.simulation.getActiveNormRequirements("block").flatMap { req =>
      if (observation.attached.size > req.quantity)
        Some(new DetachBlocksIntention())
      else None
    }

    val taskIntentions = if (knownGoalZones.nonEmpty) {
      observation.simulation.getTasks.map { task =>
        val goal = findClosestFreeGoalZone(observation.currentPos, knownGoalZones, task)
        new CompleteTaskIntention(task, goal)
      }
    } else Seq.empty

    val roleIntentions = observation.simulation.getReservedRoles().get(observation.agentId)
      .filterNot(role => observation.currentRole.contains(role.name))
      .map(role => new AdoptRoleIntention(role.name)).toSeq

    val detachIfOverloaded = if (observation.attached.size > 2) Seq(new DetachBlocksIntention()) else Seq()

    taskIntentions ++ roleIntentions ++ detachIfOverloaded
  }

  private def findClosestFreeGoalZone(currentPos: Coordinate, goalZones: Set[Coordinate], task: Task): Coordinate = {
    //TODO consider changing to avoid crowded zones
    goalZones.minBy(_.distanceTo(currentPos))
  }

  private def violatesRoleNorm(observation: Observation): Boolean = {
    val reservedRoles = observation.simulation.getReservedRoles()
    val activeRoleNorms = observation.simulation.getActiveNormRequirements("role")

    activeRoleNorms.exists { norm =>
      val agentsWithThisRole = reservedRoles.values.count(_.name == norm.name)
      agentsWithThisRole > norm.quantity && observation.currentRole.contains(norm.name)
    }
  }




}
