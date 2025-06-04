package shared
import model.{Coordinate, Thing}
import scala.collection.mutable

// 3. Message for sharing mapped coordinates
case class ShareMap(
                     senderName: String,
                     senderStep: Int,
                     translatedMap: Map[Coordinate, Thing]
                   )