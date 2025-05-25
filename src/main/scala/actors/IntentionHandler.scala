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
    // 1. Handle norm violations FIRST
    if (violatesRoleNorm(observation)) {
      val fallbackRoles = observation.simulation.getAllowedFallbackRoles(observation.currentRole)
      if (fallbackRoles.nonEmpty) {
        currentIntention = Some(new AdoptRoleIntention(fallbackRoles.head))
      }
      return
    }


    // 2. Build scored intention candidates
    val candidates = fallbackIntentions ++ discoverNewIntentions(observation)
    val best = candidates.maxBy(_.score(observation))

    val currentScore = currentIntention.map(_.score(observation)).getOrElse(Double.MinValue)

    // 3. Only switch if current intention is done or clearly worse
    val shouldSwitch =
      currentIntention.isEmpty ||
        currentIntention.exists(_.checkFinished(observation)) ||
        best.score(observation) > currentScore + hysteresis

    if (shouldSwitch) {
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
    val old = currentIntention
    evaluateAndMaybeSwitch(observation)
    val oldName = old.map(_.getClass.getSimpleName).getOrElse("None")
    val currentName = currentIntention.map(_.getClass.getSimpleName).getOrElse("None")
    println(s"${observation.agentId} Previous: $oldName Current: $currentName")
    currentIntention.map(_.planNextAction(observation)).getOrElse(SkipAction())
  }

  private def getTaskIntentionIfVisible(observation: Observation): Seq[ScoredIntention] = {
    observation.simulation.getTasks.map(t => new CompleteTaskIntention(t, Coordinate(0,0)))
  }

  private def discoverNewIntentions(observation: Observation): Seq[ScoredIntention] = {
    val knownGoalZones = observation.getKnownGoalZones
    val knownRoleZones = observation.getKnownRoleZones
    val currentRole = observation.currentRole.getOrElse("default")

    // Norm-based fallback intentions
    val normIntentions = observation.simulation.getActiveNormRequirements("role").flatMap { req =>
      if (currentRole == req.name) Some(new AdoptRoleIntention("default")) else None
    } ++ observation.simulation.getActiveNormRequirements("block").flatMap { req =>
      if (observation.attached.size > req.quantity) Some(new DetachBlocksIntention()) else None
    }

    // If agent knows role zones but no goal zones and has a worse role (like "default"), adopt a better one
    val adoptBetterRoleIfIdle: Seq[ScoredIntention] =
      if (knownGoalZones.isEmpty && knownRoleZones.nonEmpty && currentRole == "default") {
        val fallback = observation.simulation.getAllowedFallbackRoles(Some("default"))
        fallback.headOption.map(role => new AdoptRoleIntention(role)).toSeq
      } else Seq.empty

    // Task-related intentions
    val taskIntentions = if (knownGoalZones.nonEmpty) {
      observation.simulation.getTasks.map { task =>
        val goal = findClosestFreeGoalZone(observation.currentPos, knownGoalZones, task)
        new CompleteTaskIntention(task, goal)
      }
    } else Seq.empty

    // Reserved role adoption (optional logic — possibly deprecated)
    val roleIntentions = observation.simulation.getReservedRoles().get(observation.agentId)
      .filterNot(role => observation.currentRole.contains(role.name))
      .map(role => new AdoptRoleIntention(role.name)).toSeq

    val detachIfOverloaded = if (observation.attached.size > 2) Seq(new DetachBlocksIntention()) else Seq()

    // Final aggregation
    normIntentions ++ adoptBetterRoleIfIdle ++ taskIntentions ++ roleIntentions ++ detachIfOverloaded
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
