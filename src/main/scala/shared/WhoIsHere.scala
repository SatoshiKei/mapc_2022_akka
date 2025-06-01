package shared

import model.{Coordinate, Thing}
//import scala.collection.mutable

// 1. Message to identify self and share percept for alignment
case class WhoIsHere(
                      senderName: String,
                      senderStep: Int,
                      senderGlobalPos: Coordinate,
                      senderPercept: Vector[Thing]  // relative to sender
                    )