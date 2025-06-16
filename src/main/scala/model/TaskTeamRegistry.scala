package model

case class TaskTeamRegistry(
                               taskId: String,
                               assemblies: List[TaskAssembly]
                             ) {


//  def mergeWith2(remote: TaskTeamRegistry, maxParticipants: Int): TaskTeamRegistry = {
//    require(this.taskId == remote.taskId, "Mismatched task IDs")
//    val combined = this.assemblies ++ remote.assemblies
//    val allParticipants = mergeParticipants(combined)
//    var newAssemblies = List.empty
//    var assembly: TaskAssembly = None
//    allParticipants.foreach(p =>
//      if (assembly.participants.size == maxParticipants) {
//        newAssemblies +: TaskAssembly()
//        assembly
//      } else {
//
//      }
//    )
//
//    TaskTeamRegistry(taskId, newAssemblies)
//  }

  def mergeWith(remote: TaskTeamRegistry, maxParticipants: Int): TaskTeamRegistry = {
    require(this.taskId == remote.taskId, "Mismatched task IDs")

    val combined = this.assemblies ++ remote.assemblies
    val latestUpdate = combined.map(_.lastUpdated).max

    val allParticipants = mergeParticipants(combined)
    val allAssignments = mergeAssignments(combined)

    val grouped = allParticipants.grouped(maxParticipants).toList
    val newAssemblies = createAssemblies(grouped, allAssignments, latestUpdate, combined)

    TaskTeamRegistry(taskId, newAssemblies)
  }

  // Remove duplicates, preserve earliest joinedStep and lowest ID
  private def mergeParticipants(all: List[TaskAssembly]): List[Participant] = {
    val flat = all.flatMap(_.participants)
    val grouped = flat.groupBy(_.name)
    grouped.mapValues(_.minBy(_.joinedStep)).values.toList
      .sortBy(p => (p.joinedStep, extractId(p.name)))
  }

  // Merge and deduplicate assignments
  private def mergeAssignments(all: List[TaskAssembly]): List[(Coordinate, String)] = {
    val deduped = scala.collection.mutable.LinkedHashMap[Coordinate, String]()
    all.flatMap(_.participantAssignments).foreach { a =>
      if (!deduped.contains(a.coordinate)) {
        deduped.put(a.coordinate, a.blockType)
      }
    }
    deduped.toList
  }

  // Assign requirements to agents in order
  private def createAssemblies(
                                groups: List[List[Participant]],
                                assignments: List[(Coordinate, String)],
                                step: Int,
                                assemblies: List[TaskAssembly]
                              ): List[TaskAssembly] = {
    var result = List.empty[TaskAssembly]

    for ((group, idx) <- groups.zipWithIndex) {
      val sorted = group.sortBy(p => (p.joinedStep, extractId(p.name)))
      val recipient = sorted.head

      // Try to preserve recipient's SharedCoordinate if known
      val baseZone = assemblies.find(_.participants.exists(_.name == recipient.name))
        .map(_.goalZone)
        .getOrElse(SharedCoordinate(0 + idx, 0 + idx, recipient.name))

      val reassigned = assignments.zipWithIndex.map {
        case ((coord, typ), i) =>
          val agent = sorted(i % sorted.size)
          ParticipantAssignment(agent.name, coord, typ)
      }.toSet

      result ::= TaskAssembly(
        goalZone = SharedCoordinate(baseZone.x, baseZone.y, recipient.name),
        participants = group.toSet,
        participantAssignments = reassigned,
        lastUpdated = step
      )
    }

    result.reverse
  }

  // Extract numeric part from name; fallback to Int.MaxValue
  private def extractId(name: String): Int = {
    try name.replaceAll("[^0-9]", "").toInt
    catch { case _: Exception => Int.MaxValue }
  }

  def getAssembly(agentId: String): Option[TaskAssembly] =
    assemblies.find(_.allAgents.contains(agentId))


}
