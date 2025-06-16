package model

case class TaskAssembly(
                         goalZone: SharedCoordinate,
                         participants: Set[Participant],
                         participantAssignments: Set[ParticipantAssignment],
                         lastUpdated: Int
                       ) {

  def allAgents: Set[String] = participants.map(_.name)

  def recipient: String = {
    participants.toList.sortWith { (a, b) =>
      if (a.joinedStep != b.joinedStep) a.joinedStep < b.joinedStep
      else extractAgentId(a.name) < extractAgentId(b.name)
    }.head.name
  }

  def supporters: Set[String] = allAgents - recipient

  private def extractAgentId(name: String): Int = {
    try {
      name.replaceAll("[^0-9]", "").toInt
    } catch {
      case _: NumberFormatException => Int.MaxValue
    }
  }
}
