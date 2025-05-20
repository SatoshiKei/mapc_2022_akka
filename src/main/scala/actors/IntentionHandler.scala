package actors

import intentions._
import model._

import scala.collection.mutable.PriorityQueue

class IntentionHandler() {

  private val intentions = PriorityQueue.empty[ScoredIntention](Ordering.by(-_.priority))

  var currentIntention: Option[Intention] = None

  // Insert base intentions like Explore and Idle
  def initialize(): Unit = {
    insertIntention(new ExploreIntention())
    insertIntention(new IdleIntention())
  }

  def insertIntention(intention: Intention): Unit = {
    intentions.enqueue(ScoredIntention(intention, intentions.size))
  }

  def getCurrentIntention(observation: Observation): Option[Intention] = {
    currentIntention match {
      case Some(intent) if !intent.checkFinished(observation) => Some(intent)
      case _ =>
        if (intentions.nonEmpty) {
          val next = intentions.dequeue().intention
          currentIntention = Some(next)
          Some(next)
        } else {
          None
        }
    }
  }


  def planNextAction(observation: Observation): AgentAction = {
    val intention: Intention = getCurrentIntention(observation).getOrElse(new IdleIntention())
    intention.planNextAction(observation)
  }

  // Utility class to prioritize intentions
  case class ScoredIntention(intention: Intention, priority: Double)
}
