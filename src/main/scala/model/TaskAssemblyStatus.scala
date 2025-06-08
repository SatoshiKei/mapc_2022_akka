package model

case class TaskAssemblyStatus(
                               taskId: String,
                               goalZone: Coordinate,
                               blockAssignments: Map[Coordinate, (String, String)],
                               recipient: Option[String],
                               committedAgents: Set[String],
                               lastUpdated: Int
                             )
