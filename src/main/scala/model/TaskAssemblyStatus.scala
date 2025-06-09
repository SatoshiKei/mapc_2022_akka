package model

case class TaskAssemblyStatus(
                               taskId: String,
                               assemblies: List[TaskAssembly]
                             ) {

  def mergeWith(remote: TaskAssemblyStatus, maxParticipants: Int): TaskAssemblyStatus = {
    require(this.taskId == remote.taskId, "Mismatched task IDs")

    // Merge all assemblies together without grouping by goal
    val combined = this.assemblies ++ remote.assemblies

    val allAgents = combined.flatMap(_.allAgents).toSet
    val allAssignments = combined.flatMap(_.blockAssignments).toMap
    val latestUpdate = combined.map(_.lastUpdated).max

    val agentList = allAgents.toList.sorted
    val primaryAgents = agentList.take(maxParticipants)
    val overflowAgents = agentList.drop(maxParticipants)

    val primaryRecipient = primaryAgents.head
    val primaryCommitted = primaryAgents.tail.toSet
    val primaryGoal = combined.head.goalZone // Just use any of the original goals

    val primaryAssembly = TaskAssembly(
      goalZone = primaryGoal,
      recipient = primaryRecipient,
      committedAgents = primaryCommitted,
      blockAssignments = allAssignments.filter { case (_, (agent, _)) => primaryAgents.contains(agent) },
      lastUpdated = latestUpdate
    )

    val secondaryAssemblies = overflowAgents.grouped(maxParticipants).zipWithIndex.map {
      case (agents, i) =>
        val rec = agents.head
        val committed = agents.tail.toSet
        val goal = Coordinate(primaryGoal.x + (i + 1), primaryGoal.y + (i + 1)) // deterministic distinct zones
        TaskAssembly(
          goalZone = goal,
          recipient = rec,
          committedAgents = committed,
          blockAssignments = allAssignments.filter { case (_, (agent, _)) => agents.contains(agent) },
          lastUpdated = latestUpdate
        )
    }.toList

    TaskAssemblyStatus(taskId, primaryAssembly :: secondaryAssemblies)
  }




    def getAssembly(agentId: String) = {
      assemblies.find(a => a.allAgents.contains(agentId))
    }

}
