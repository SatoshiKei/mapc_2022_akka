package shared
import model.Coordinate
import scala.collection.mutable

// 3. Message for sharing mapped coordinates
case class ShareMap(
                     senderName: String,
                     senderStep: Int,
                     translatedMap: mutable.Map[Coordinate, String] // already translated to receiver's system
                   )