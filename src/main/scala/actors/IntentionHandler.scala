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
    observation.simulation.getTasks.map(t => new CompleteTaskIntention(t, Coordinate(0,0))) //TO DO Task Goal
  }

  private def discoverNewIntentions(observation: Observation): Seq[ScoredIntention] = {
    val taskIntentions: Seq[ScoredIntention] = observation.simulation.getTasks.map { task =>
      new CompleteTaskIntention(task, Coordinate(0,0))
    }

    val roleIntentions: Seq[ScoredIntention] = observation.simulation.getReservedRoles().get(observation.agentId)
      .filterNot(role => observation.currentRole.contains(role.name))
      .map(role => new AdoptRoleIntention(role.name)).toSeq

    val detachIfOverloaded: Seq[ScoredIntention] = if (observation.attached.size > 2)
      Seq(new DetachBlocksIntention()) else Seq()

    taskIntentions ++ roleIntentions ++ detachIfOverloaded
  }

}
