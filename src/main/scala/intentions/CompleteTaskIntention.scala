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



    // Step 0: Get the TaskAssemblyStatus (and commit if needed)
    val status = getOrInitTask(observation)
    val assembly: TaskAssembly = status.getAssembly(observation.agentId) match {
      case Some(value) =>
        value
      case None =>
        println(observation.agentId + " fails as there was no task assembly ready at CompleteTaskIntention")
        return SkipAction()
    }

    // Step 1: Find all attachable requirement coordinates
    val attachableOffsets = Set(Coordinate(0, 1), Coordinate(0, -1), Coordinate(1, 0), Coordinate(-1, 0))

    val candidateAssignments = task.requirements.filter { req =>
      attachableOffsets.contains(req.coordinate)
    }

    // Step 2: Try to pick one assigned to this agent, or unassigned
    val requirement = candidateAssignments.find { req =>
      assembly.blockAssignments.get(req.coordinate) match {
        case Some((assignedType, assignedAgent)) => assignedAgent == observation.agentId
        case None => true
      }
    }

    // Step 2: If no adjacent block found, fallback to Explore for now
    if (requirement.isEmpty ) {
      println(s"${observation.agentId} found no immediate attachable block for ${task.name}")
      if (!subIntention.exists(_.isInstanceOf[ExploreIntention])) subIntention = Some(new ExploreIntention())
      return subIntention.get.planNextAction(observation)
    }

    // Step 3: Delegate to AttachFirstBlockIntention for the correct block type
    val blockType = requirement.get.`type`
    if ((subIntention.isEmpty || !subIntention.get.isInstanceOf[AttachFirstBlockIntention]) && observation.attached.isEmpty) {
      println(observation.agentId + " is pursuing requirement for " + task.name + " for recepient " + assembly.recipient + " with " + assembly.committedAgents + " as helpers")
      subIntention = Some(new AttachFirstBlockIntention(blockType, requirement.get.coordinate))
    }

    //Step 5: Deliver the task
    if (observation.attached.size == task.requirements.size) {

      if (!observation.simulation.getRolesWithAction("submit").contains(observation.currentRole.getOrElse("standard"))) {
        println(observation.agentId + " is looking for role to submit task")
        subIntention = Some(new AdoptRoleIntention(observation.simulation.getRolesWithAction("submit").headOption.get))
      } else {
        //TODO - Goal Zones can change, calculate them dynamically
        //val goalZoneOpt = observation.getKnownGoalZones.minByOption(_.distanceTo(observation.currentPos))
        if (observation.currentPos == goalZone) {
          SubmitAction(task.name)
        } else {
          println(observation.agentId + "is traveling to a goal zone at " + goalZone)
          subIntention = Some(new TravelIntention(goalZone))
        }
      }

    }

    subIntention match {
      case Some(intention: Intention) =>
        intention.planNextAction(observation)
      case None =>
        println(observation.agentId + " is exploring because they cannot complete task " + task.name)
        new ExploreIntention().planNextAction(observation)
    }
  }

  private def getOrInitTask(observation: Observation): TaskAssemblyStatus = {
    val taskId = task.name
    val agentId = observation.agentId
    val requiredBlocks = task.requirements.size
    val step = observation.simulation.getSimulationStep

    // Helper to create a new assembly instance
    def createNewAssembly(): TaskAssembly = {
      TaskAssembly(
        goalZone = goalZone,
        blockAssignments = Map.empty,
        recipient = agentId,
        committedAgents = Set.empty,
        lastUpdated = step
      )
    }

    // Fetch current TaskAssemblyStatus or initialize if missing
    val currentStatus = observation.taskStatus.getOrElseUpdate(
      taskId,
      TaskAssemblyStatus(
        taskId = taskId,
        assemblies = List.empty
      )
    )

    // Try to find a joinable assembly (not full and not already joined)
    val maybeJoinable = currentStatus.assemblies.find { assembly =>
      val totalParticipants = assembly.committedAgents.size + 1
      totalParticipants < requiredBlocks && !assembly.allAgents.contains(agentId)
    }

    val updatedAssemblies = maybeJoinable match {
      case Some(joinable) =>
        // Join this existing assembly
        val updated = joinable.copy(
          committedAgents = joinable.committedAgents + agentId,
          lastUpdated = step
        )
        currentStatus.assemblies.map(a => if (a == joinable) updated else a)

      case None =>
        // No room in existing assemblies, start a new one
        currentStatus.assemblies :+ createNewAssembly()
    }

    // Store updated status
    val updatedStatus = currentStatus.copy(assemblies = updatedAssemblies)
    observation.taskStatus.update(taskId, updatedStatus)
    updatedStatus
  }


  override def checkFinished(observation: Observation): Boolean = {
    blockPlan.isEmpty
  }



}
