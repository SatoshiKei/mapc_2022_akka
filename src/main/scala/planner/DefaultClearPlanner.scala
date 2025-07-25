package planner

import action.ClearAction
import model.{AgentAction, Coordinate, Observation}

class DefaultClearPlanner(minEnergy: Int = 10) extends ClearPlanner {

  override def shouldClear(observation: Observation, direction: String): Option[AgentAction] = {
    val rel = Coordinate.fromDirection(direction)

    val clearable = observation.things.find(t => {
      val abs = observation.currentPos + Coordinate(t.x, t.y)
      t.x == rel.x && t.y == rel.y &&
        (t.`type` == "obstacle" || t.`type` == "block") &&
        t.details.isEmpty &&
        !observation.globalMap.get(abs).exists(thing => thing.`type` == "entity")
    })


    val energy = observation.energy

    if (clearable.isDefined && energy >= minEnergy) {
      Some(ClearAction(rel))
    } else {
      None
    }
  }
}
