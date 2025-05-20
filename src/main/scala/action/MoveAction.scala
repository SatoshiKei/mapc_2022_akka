package action

import model.AgentAction

object MoveAction {
  /**
   * Creates a move action in the specified direction.
   *
   * @param direction A string representing one of: "n", "s", "e", "w"
   * @return An AgentAction representing the move
   */
  def apply(direction: String): AgentAction = {
    require(Set("n", "s", "e", "w").contains(direction), s"Invalid move direction: $direction")
    AgentAction("move", Seq(direction))
  }
}
