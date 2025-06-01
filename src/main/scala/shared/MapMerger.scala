package shared
import model.Coordinate
import scala.collection.mutable

object MapMerger {
  def merge(receiverMap: mutable.Map[Coordinate, String], incoming: mutable.Map[Coordinate, String]): Unit = {
    for ((coord, value) <- incoming) {
      if (!receiverMap.contains(coord)) {
        receiverMap.update(coord, value)
      }
    }
  }
}