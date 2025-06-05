package shared
import model.{Coordinate, Thing, Zone}

import scala.collection.mutable

object MapMerger {
  def merge(receiverMap: mutable.Map[Coordinate, Thing], incoming: Map[Coordinate, Thing]): Int = {
    var totalUpdates = 0
    for ((coord, value) <- incoming) {
      receiverMap.get(coord) match {
        case Some(existingThing) =>
          if (value.step > existingThing.step) {
            receiverMap.update(coord, value)
            totalUpdates += 1
          }
        case None =>
          receiverMap.update(coord, value)
          totalUpdates += 1
      }
    }
    totalUpdates
  }

  def mergeZones(local: mutable.Map[Coordinate, Zone], incoming: Map[Coordinate, Zone]): Int = {
    var count = 0
    for ((coord, zone) <- incoming) {
      local.get(coord) match {
        case Some(existing) if zone.step > existing.step =>
          local.update(coord, zone)
          count += 1
        case None =>
          local.update(coord, zone)
          count += 1
        case _ =>
      }
    }
    count
  }

}