package model

case class Role(
                 name: String,
                 vision: Int,
                 actions: Set[String],
                 speed: Vector[Integer],
                 clear: ClearAbility
               ) {

  def canPerformAction(action: String): Boolean = {
    actions.contains(action.toLowerCase)
  }

  def getSpeed(attachedCount: Int): Int = {
    val index = if (attachedCount >= speed.length) speed.length - 1 else attachedCount
    math.min(speed(index), 2)
  }

  override def toString: String = name
}

  case class ClearAbility(
                           chance: Double,
                           maxDistance: Int
                         )
