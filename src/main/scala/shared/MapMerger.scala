package shared
import model.{Coordinate, Thing}
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
}