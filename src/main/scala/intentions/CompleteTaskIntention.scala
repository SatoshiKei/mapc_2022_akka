package intentions

import action.{AttachAction, DetachAction, SkipAction, SubmitAction}
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
    val registry = getOrInitTask(observation)
    var assembly: TaskAssembly = registry.getAssembly(observation.agentId) match {
      case Some(value) =>
        value
      case None =>
        println(observation.agentId + " fails as there was no task assembly ready at CompleteTaskIntention")
        return SkipAction()
    }

    val hasAssignment = assembly.participantAssignments.exists(_.name == observation.agentId)

    if (!hasAssignment) {
      val assignedCoords = assembly.participantAssignments.map(_.coordinate).toSet
      val unassigned = task.requirements.filterNot(r => assignedCoords.contains(r.coordinate))

      unassigned.headOption.foreach { free =>
        val updatedAssignment = ParticipantAssignment(observation.agentId, free.coordinate, free.`type`)
        val updatedAssignments = assembly.participantAssignments + updatedAssignment

        val updatedAssembly = assembly.copy(participantAssignments = updatedAssignments)
        val updatedAssemblies = registry.assemblies.map {
          case a if a == assembly => updatedAssembly
          case other => other
        }
        assembly = updatedAssembly
        val updatedRegistry = registry.copy(assemblies = updatedAssemblies)
        observation.taskRegistry.update(task.name, updatedRegistry)

        println(s"${observation.agentId} assigned missing requirement ${free.coordinate} for ${task.name}")
      }
    }

    // Step 1: Find all attachable requirement coordinates
    //val attachableOffsets = Set(Coordinate(0, 1), Coordinate(0, -1), Coordinate(1, 0), Coordinate(-1, 0))

//    val candidateAssignments = task.requirements.filter { req =>
//      attachableOffsets.contains(req.coordinate)
//    }

    // Step 2: Try to pick one assigned to this agent, or unassigned
    val requirement: TaskRequirement = {
      val assignment = assembly.participantAssignments.find(_.name == observation.agentId).get
      task.requirements.find(_.coordinate == assignment.coordinate).get
    }

    if (observation.attached.nonEmpty && !observation.isBlockAttached(requirement.`type`)) {
      println(observation.agentId + "Detaching block to search for new requirement: " + requirement.`type`)
      return DetachAction(Coordinate.toDirection(observation.attached.head).get)
    }


    // Step 2: If no adjacent block found, fallback to Explore for now
//    if (requirement.isEmpty ) {
//      println(s"${observation.agentId} found no immediate attachable block for ${task.name}")
//      if (!subIntention.exists(_.isInstanceOf[ExploreIntention])) subIntention = Some(new ExploreIntention())
//      return subIntention.get.planNextAction(observation)
//    }

    // Step 3: Delegate to AttachFirstBlockIntention for the correct block type
    val blockType = requirement.`type`
    if ((subIntention.isEmpty || !subIntention.get.isInstanceOf[AttachFirstBlockIntention]) && observation.attached.isEmpty) {
//      println(observation.agentId + " is pursuing requirement for " + task.name + " for recepient " + assembly.recipient + " with " + assembly.committedAgents + " as helpers")
      subIntention = Some(new AttachFirstBlockIntention(blockType, requirement.coordinate))
    }

    if (assembly != null) {
      println(observation.agentId + " is pursuing requirement for " + task.name + " for recepient " + assembly.recipient + " with " + assembly.supporters + " as supporters")
    }

    //Step 5: Deliver the task
    if (observation.attached.nonEmpty) {

      if (!observation.simulation.getRolesWithAction("submit").contains(observation.currentRole.getOrElse("standard"))) {
        println(observation.agentId + " is looking for role to submit task")
        subIntention = Some(new AdoptRoleIntention(observation.simulation.getRolesWithAction("submit").headOption.get))
      } else {
        val goal = observation.translateRemoteCoordinate(assembly.goalZone).getOrElse(goalZone)
        println(observation.agentId + " ")
        if (observation.knownGoalZones.contains(observation.currentPos)) {
          val allRequirementsMet = observation.allRequirementsMet(task)
          if (allRequirementsMet) {
            SubmitAction(task.name)
          } else {
            subIntention = Some(new ConnectBlocksIntention(task, goal))
          }
        } else {
          println(observation.agentId + "is traveling to a goal zone at " + goal)
          subIntention = Some(new TravelIntention(goal))
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

  private def getOrInitTask(observation: Observation): TaskTeamRegistry = {
    val taskId = task.name
    val agentId = observation.agentId
    val requiredBlocks = task.requirements.size
    val step = observation.simulation.getSimulationStep

    val newParticipant = Participant(agentId, step)

    def createNewAssembly(): TaskAssembly = {
      println(observation.agentId + " is creating a new assembly at " + observation.simulation.getSimulationStep + " | registry: " + observation.taskRegistry)
      val goal = SharedCoordinate(goalZone.x, goalZone.y, agentId)
      TaskAssembly(
        goalZone = goal,
        participants = Set(newParticipant),
        participantAssignments = Set.empty,
        lastUpdated = step
      )
    }

    val currentStatus = observation.taskRegistry.getOrElseUpdate(
      taskId,
      TaskTeamRegistry(taskId = taskId, assemblies = List.empty)
    )

    // If the agent is already part of an assembly, return as-is (no update)
    if (currentStatus.assemblies.exists(_.allAgents.contains(agentId))) {
      return currentStatus
    }

    // Try to find a joinable assembly that has space
    val maybeJoinable = currentStatus.assemblies.find(_.participants.size < requiredBlocks)

    val updatedAssemblies = maybeJoinable match {
      case Some(joinable) =>
        val updatedAssembly = joinable.copy(
          participants = joinable.participants + newParticipant
          // Do NOT change lastUpdated
        )
        currentStatus.assemblies.map { a =>
          if (a == joinable) updatedAssembly else a
        }

      case None =>
        currentStatus.assemblies :+ createNewAssembly()
    }

    val updatedStatus = currentStatus.copy(assemblies = updatedAssemblies)
    observation.taskRegistry.update(taskId, updatedStatus)
    updatedStatus
  }






  override def checkFinished(observation: Observation): Boolean = {
    blockPlan.isEmpty
  }



}
