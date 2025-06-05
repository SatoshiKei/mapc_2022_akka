package shared
import model.{Coordinate, Thing, Zone}

import scala.collection.mutable

// 3. Message for sharing mapped coordinates
case class ShareMap(
                     senderName: String,
                     senderStep: Int,
                     translatedMap: Map[Coordinate, Thing],
                     translatedGoalZones: Map[Coordinate, Zone],
                     translatedRoleZones: Map[Coordinate, Zone],
                     translatedKnownAgents: Map[String, KnownAgent]
                   )