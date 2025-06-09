package model

case class TaskAssembly(
                         goalZone: Coordinate,
                         recipient: String,
                         committedAgents: Set[String],
                         blockAssignments: Map[Coordinate, (String, String)], // relPos -> (agentId, blockType)
                         lastUpdated: Int
                       ) {

  def allAgents: Set[String] = {
    committedAgents ++ Some(recipient).toSet
  }

    def mergeWith(other: TaskAssembly): TaskAssembly = {

      val newer = if (this.lastUpdated >= other.lastUpdated) this else other

      // Union of committed agents
      val mergedCommitted = this.committedAgents ++ other.committedAgents

      // Keep block assignments from the newer one
      val mergedAssignments =
        if (this.lastUpdated >= other.lastUpdated) this.blockAssignments
        else other.blockAssignments

      // Prefer recipient from the newer one (non-empty)
      val mergedRecipient = Some(newer.recipient).orElse(Some(this.recipient)).orElse(Some(other.recipient))

      newer.copy(
        blockAssignments = mergedAssignments,
        committedAgents = mergedCommitted,
        recipient = mergedRecipient.get
      )
    }

}