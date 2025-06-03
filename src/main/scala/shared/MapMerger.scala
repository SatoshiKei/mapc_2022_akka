package shared
import model.{Coordinate, Thing}
import scala.collection.mutable

object MapMerger {
  def merge(receiverMap: mutable.Map[Coordinate, Thing], incoming: mutable.Map[Coordinate, Thing]): Int = {
    var totalUpdates = 0
    for ((coord, value) <- incoming) {
      if (!receiverMap.contains(coord)) {
        totalUpdates +=1
        receiverMap.update(coord, value)
      }
    }
    totalUpdates
  }
}