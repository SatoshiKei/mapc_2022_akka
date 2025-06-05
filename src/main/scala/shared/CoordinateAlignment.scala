package shared

import model.{Coordinate, Thing}

object CoordinateAlignment {

  def findOffset(mine: Vector[Thing], theirs: Vector[Thing]): Option[Coordinate] = {
    val myEntities = mine.filter(_.`type` == "entity") //TODO - Check if they belong to the same team
    val theirEntities = theirs.filter(_.`type` == "entity")

    val myThingsMap = mine.map(t => Coordinate(t.x, t.y) -> t).toMap
    val theirThingsMap = theirs.map(t => Coordinate(t.x, t.y) -> t).toMap

    // Dynamically compute bounds (vision radius)
    def perceptRadius(things: Vector[Thing]): Int = {
      things.map(t => math.abs(t.x) + math.abs(t.y)).reduceOption((a, b) => a max b).getOrElse(0)
    }

    val myRadius = perceptRadius(mine)
    val theirRadius = perceptRadius(theirs)

    val candidateOffsets = for {
      myEntity <- myEntities
      theirEntity <- theirEntities
      myCoord = Coordinate(myEntity.x, myEntity.y)
      theirCoord = Coordinate(theirEntity.x, theirEntity.y)
      if myCoord.x + theirCoord.x == 0 && myCoord.y + theirCoord.y == 0  // verify mirrored positions
    } yield Coordinate(-theirCoord.x, -theirCoord.y)  // offset from their map into mine

    candidateOffsets.find { offset =>
      // Translate all their coordinates into my map using the candidate offset
      val translatedTheirMap = theirThingsMap.map { case (coord, thing) =>
        coord + offset -> thing
      }

      // Build a set of all coordinates that are within *both* percept bounds
      val myVisibleCoords = (-myRadius to myRadius).flatMap { dx =>
        (-myRadius to myRadius).flatMap { dy =>
          val coord = Coordinate(dx, dy)
          if (math.abs(dx) + math.abs(dy) <= myRadius) Some(coord) else None
        }
      }.toSet

      val theirVisibleCoords = (-theirRadius to theirRadius).flatMap { dx =>
        (-theirRadius to theirRadius).flatMap { dy =>
          val coord = Coordinate(dx, dy) + offset
          if (math.abs(dx) + math.abs(dy) <= theirRadius) Some(coord) else None
        }
      }.toSet

      val sharedCoords = myVisibleCoords.intersect(theirVisibleCoords)

      // For each shared coordinate, compare things or empty status
      sharedCoords.forall { coord =>
        val myThingOpt = myThingsMap.get(coord)
        val theirThingOpt = translatedTheirMap.get(coord)

        (myThingOpt, theirThingOpt) match {
          case (None, None) => true
          case (Some(a), Some(b)) => a.`type` == b.`type` && a.details == b.details
          case _ => false
        }
      }
    }
  }
}
