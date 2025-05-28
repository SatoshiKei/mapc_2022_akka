package model

case class TaskRequirement(
                 x: Int,
                 y: Int,
                 details: String,
                 `type`: String,
               ) {

  def coordinate: Coordinate = Coordinate(x, y)
}
