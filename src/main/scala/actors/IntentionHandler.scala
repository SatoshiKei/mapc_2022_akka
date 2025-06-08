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
//    println("Best candidates: " + best.score(observation) + " Current score: " + currentScore)
    var logScores = ""
    candidates.foreach(intention => {
      logScores = logScores + " " + intention.getClass + ":" + intention.score(observation)
    })
    println(logScores)

    // 3. Only switch if current intention is done or clearly worse
    val shouldSwitch =
      currentIntention.isEmpty ||
        currentIntention.exists(_.checkFinished(observation)) ||
        best.score(observation) > currentScore + hysteresis

    if (shouldSwitch) {
      currentIntention = Some(best)
    }
  }


  def planNextAction(observation: Observation): AgentAction = {
    val previous = currentIntention.map(_.explain()).getOrElse("None")
    evaluateAndMaybeSwitch(observation)
//    val oldName = old.map(_.getClass.getSimpleName).getOrElse("None")
    println(s"${observation.agentId} Previous: ${previous} Current: ${currentIntention.map(_.explain()).getOrElse("None")}")
    currentIntention.map(_.planNextAction(observation)).getOrElse(SkipAction())
  }

  private def getTaskIntentionIfVisible(observation: Observation): Seq[ScoredIntention] = {
    observation.simulation.getTasks.map(t => new CompleteTaskIntention(t, Coordinate(0,0)))
  }

  private def discoverNewIntentions(observation: Observation): Seq[ScoredIntention] = {

    // Norm-based fallback intentions
    val normIntentions = discoverNormIntentions(observation)

    // If agent knows role zones but no goal zones and has a worse role (like "default"), adopt a better one
    val adoptBetterRoleIfIdle: Seq[ScoredIntention] = discoverRoleUpgradeIntentions(observation)

    // Task-related intentions
    val taskIntentions = discoverTaskIntentions(observation)

    // Reserved role adoption (optional logic — possibly deprecated)
    val roleIntentions = observation.simulation.getReservedRoles().get(observation.agentId)
      .filterNot(role => observation.currentRole.contains(role.name))
      .map(role => new AdoptRoleIntention(role.name)).toSeq

    val detachIfOverloaded = if (observation.attached.size > 2) Seq(new DetachBlocksIntention()) else Seq()

    // Final aggregation
    normIntentions ++ adoptBetterRoleIfIdle ++ taskIntentions ++ roleIntentions ++ detachIfOverloaded
  }

  private def discoverNormIntentions(obs: Observation): Seq[ScoredIntention] = {
    val currentRole = obs.currentRole.getOrElse("default")
    //TODO - Not correct at all; add logic to stop role adoption
    val roleNorms = obs.simulation.getActiveNormRequirements("role").flatMap { req =>
      if (currentRole == req.name) Some(new AdoptRoleIntention("default")) else None
    }

    val blockNorms = obs.simulation.getActiveNormRequirements("block").flatMap { req =>
      if (obs.attached.size > req.quantity) Some(new DetachBlocksIntention()) else None
    }

    roleNorms ++ blockNorms
  }

  private def discoverTaskIntentions(obs: Observation): Seq[ScoredIntention] = {
    if (obs.getKnownGoalZones.nonEmpty) {
      obs.simulation.getTasks.map { task =>
        val goal = obs.getKnownGoalZones.minBy(_.distanceTo(obs.currentPos))
        new CompleteTaskIntention(task, goal)
      }
    } else Seq.empty
  }

  private def discoverRoleUpgradeIntentions(obs: Observation): Seq[ScoredIntention] = {
    val currentRole = obs.currentRole.getOrElse("default")
    if (obs.getKnownRoleZones.nonEmpty && obs.getKnownGoalZones.isEmpty && currentRole == "default") {
      println(obs.agentId + " intends to adopt a better role")
      val betterRoles = obs.simulation.getAllowedFallbackRoles(Some("default"))
      betterRoles.headOption.map(new AdoptRoleIntention(_)).toSeq
    } else Seq.empty
  }

  private def violatesRoleNorm(observation: Observation): Boolean = {
    val reservedRoles = observation.simulation.getReservedRoles()
    val activeRoleNorms = observation.simulation.getActiveNormRequirements("role")

    activeRoleNorms.exists { norm =>
      val agentsWithThisRole = reservedRoles.values.count(_.name == norm.name)
      agentsWithThisRole > norm.quantity && observation.currentRole.contains(norm.name)
    }
  }

  private def findClosestFreeGoalZone(currentPos: Coordinate, goalZones: Set[Coordinate], task: Task): Coordinate = {
    //TODO consider changing to avoid crowded zones
    goalZones.minBy(_.distanceTo(currentPos))
  }


}
