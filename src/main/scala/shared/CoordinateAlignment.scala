package shared
import model.{Coordinate, Thing}

object CoordinateAlignment {
  def findOffset(mine: Vector[Thing], theirs: Vector[Thing]): Option[Coordinate] = {
    val sharedCandidates = for {
      myThing <- mine
      theirThing <- theirs
      if myThing.`type` == theirThing.`type` && myThing.details == theirThing.details
    } yield {
      val myCoord = Coordinate(myThing.x, myThing.y)
      val theirCoord = Coordinate(theirThing.x, theirThing.y)
      (myCoord, theirCoord)
    }

    sharedCandidates.headOption.map { case (myC, theirC) =>
      myC - theirC
    }
  }
}
