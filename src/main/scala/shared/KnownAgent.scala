package shared
import model.Coordinate
import scala.collection.mutable

case class KnownAgent(
                       name: String,
                       offset: Coordinate,
                       lastSeenStep: Int
                     )

class KnownAgents {
  val known: mutable.Map[String, KnownAgent] = mutable.Map.empty

  def update(agentName: String, offset: Coordinate, step: Int): Unit = {
    known.update(agentName, KnownAgent(agentName, offset, step))
  }

  def getOffset(agentName: String): Option[Coordinate] =
    known.get(agentName).map(_.offset)
}