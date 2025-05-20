package model

case class Role(
                 name: String,
                 vision: Int,
                 clearChance: Double,
                 clearMaxDistance: Int,
                 actions: Set[String],
                 speed: Seq[Int]
               ) {

  def canPerformAction(action: String): Boolean = {
    actions.contains(action.toLowerCase)
  }

  def getSpeed(attachedCount: Int): Int = {
    val index = if (attachedCount >= speed.length) speed.length - 1 else attachedCount
    math.min(speed(index), 2)
  }

  def getFreeSpeed: Int = {
    math.min(speed.headOption.getOrElse(1), 2)
  }

  override def toString: String = name
}
