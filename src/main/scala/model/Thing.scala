package model

case class Thing(
                  x: Int,
                  y: Int,
                  `type`: String,
                  details: String,
                  step: Int
                ) {
  def position: Coordinate = Coordinate(x, y)
}