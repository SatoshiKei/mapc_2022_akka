package shared
import model.{Coordinate, Thing}
import scala.collection.mutable

object MapMerger {
  def merge(receiverMap: mutable.Map[Coordinate, Thing], incoming: mutable.Map[Coordinate, Thing]): Unit = {
    for ((coord, value) <- incoming) {
      if (!receiverMap.contains(coord)) {
        receiverMap.update(coord, value)
      }
    }
  }
}