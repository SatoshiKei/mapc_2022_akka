package actors {

  import akka.actor.Actor
  import akka.actor.Props

  import java.io._
  import java.net.Socket
  import io.circe._
  import io.circe.parser._
  import io.circe.syntax._
  import io.circe.generic.auto._
  import model.{AgentAction, Coordinate, Observation, Role}
  import scala.collection.mutable

  object AgentActor {
    def props(agentName: String, password: String): Props = Props(new AgentActor(agentName: String, password: String))
  }

  class AgentActor(agentName: String, password: String) extends Actor {

    val socket = new Socket("localhost", 12300)
    val writer = new PrintWriter(socket.getOutputStream, true)
    val reader = new BufferedReader(new InputStreamReader(socket.getInputStream))

    var observation: Option[Observation] = None
    var intentionHandler: IntentionHandler = new IntentionHandler()
    var currentAction: Option[AgentAction] = None
    var globalReservedRoles: Map[String, Role] = Map.empty
    var globalPosition: Coordinate = Coordinate(0, 0)
    val targetPosition: Coordinate = Coordinate(0, 0)
    var globalMap: mutable.Map[Coordinate, String] = mutable.Map.empty
    var orientation: String = "n"
    var currentRole: String = "std"
    var allRoles: Vector[Role] = Vector()

    override def preStart(): Unit = {
      println("Connecting to MASSim...")
//      intentionHandler.initialize()
      // Send auth-request
      val authRequest = Json.obj(
        "type" -> Json.fromString("auth-request"),
        "content" -> Json.obj(
          "user" -> Json.fromString(agentName),
          "pw" -> Json.fromString(password)
        )
      )

      sendMessage(authRequest)

      // Start listening in a new thread
      new Thread(() => listen()).start()
    }

    def receive: Receive = {
      case jsonString: String =>
        parse(jsonString) match {
          case Right(json) =>
            val msgType = json.hcursor.get[String]("type").getOrElse("")

            msgType match {
              case "auth-response" =>
                println("✅ Authenticated.")

              case "sim-start" =>
                val percept = json.hcursor.downField("content").downField("percept")
//                println("Percept: " + json)
                val allRoles = percept.get[Vector[Role]]("roles").getOrElse(Vector())
                this.allRoles = allRoles
                println("🚀 Simulation started.")
//                println("Roles: " + roles + allRoles)
              case "request-action" =>
                val action = handleActionRequest(json)
                sendMessage(action)
                val energy = json.hcursor.downField("content").downField("percept").get[Int]("energy").getOrElse(0)
                val lastActionParams = json.hcursor.downField("content").downField("percept").get[Vector[String]]("lastActionParams").getOrElse(Vector()).mkString(",")
                val lastAction = json.hcursor.downField("content").downField("percept").get[String]("lastAction").getOrElse("")
                val lastActionResult = json.hcursor.downField("content").downField("percept").get[String]("lastActionResult").getOrElse("")

                if (lastAction == "move" && lastActionResult == "success") {
                  globalPosition = globalPosition + globalPosition.fromDirection(lastActionParams)
                }

                if (lastAction == "rotate" && lastActionResult == "success") {
                  orientation = rotateDirection(orientation, lastActionParams)
                }

//                if (lastAction == "clear" && lastActionResult == "success") {
//                  val lastActionCoordinate = globalPosition.fromDirection(lastActionParams)
//                  val dx = lastActionCoordinate.x
//                  val dy = lastActionCoordinate.y
//                  val clearedCoord = globalPosition + Coordinate(dx, dy)
//                  globalMap.update(clearedCoord, "cleared")
//                }


                println(s"Last Action: $agentName/$energy/$currentRole $globalPosition  [$lastAction/$lastActionParams -> $lastActionResult]")
                println(agentName + " is sending action: " + action.noSpaces)

              case "sim-end" =>
                println("🏁 Simulation ended.")

              case "bye" =>
                println("👋 Server closed.")
                context.stop(self)

              case _ =>
                println(s"⚠️ Unknown message: $json")
            }

          case Left(error) =>
            println(s"❌ Failed to parse JSON: $error")
        }
    }

    def sendMessage(json: Json): Unit = {
      writer.println(json.noSpaces + "\u0000")
    }

    def listen(): Unit = {
      while (true) {
        val buffer = new StringBuilder
        var c = reader.read()
        while (c != 0 && c != -1) {
          buffer.append(c.toChar)
          c = reader.read()
        }
        if (buffer.nonEmpty) {
          self ! buffer.toString()
        }
      }
    }

    import io.circe._
    import model._

    def createObservation(json: Json): Observation = {
      val cursor = json.hcursor
      val content = cursor.downField("content")
      val percept = content.downField("percept")

      // Agent info
      val agentId = agentName

      // Default position (could be updated later if available)
      val currentPos = globalPosition
//      println(agentName + " position: " + currentPos)

      // Energy
      val energy = percept.get[Int]("energy").getOrElse(0)

      // Things
      val things = parseThings(percept)
      println(agentName + "'s things: " + things)

      // Attachment
      val attached = percept.get[Vector[(Int, Int)]]("attached").getOrElse(Vector()).map { case (x, y) => Coordinate(x, y) }

      // Simulation step
      val step = content.get[Int]("step").getOrElse(0)

      // Tasks (optional)
      val tasks = percept.get[Vector[Task]]("tasks").getOrElse(Vector())

      // Current Role (optional)
      val currentRole = percept.get[String]("role").toOption
      this.currentRole = currentRole.get

      // Norms (optional)
      val norms = percept.get[Vector[Norm]]("norms").getOrElse(Vector())

      // Reserved roles (optional)
      val reservedRoles: Map[String, Role] =
        percept.get[Vector[Json]]("reservedRoles").getOrElse(Vector()).flatMap { roleJson =>
          for {
            agent <- roleJson.hcursor.get[String]("agent").toOption
            role <- roleJson.hcursor.get[Role]("role").toOption
          } yield agent -> role
        }.toMap

      // Goal Zones
      val goalZones: Set[Coordinate] =
        percept.get[Vector[(Int, Int)]]("goalZones")
          .getOrElse(Vector())
          .map { case (x, y) =>
            globalPosition + Coordinate(x, y)
          }
          .toSet

      // Role Zones
      val roleZones: Set[Coordinate] =
        percept.get[Vector[(Int, Int)]]("roleZones")
          .getOrElse(Vector())
          .map { case (x, y) =>
            globalPosition + Coordinate(x, y)
          }
          .toSet
      println("Current: " + globalPosition + "Percept: " + percept.get[Vector[(Int, Int)]]("roleZones") + " roleZones: " + roleZones)
      val sim = new Simulation(teamName = percept.get[String]("team").getOrElse("A"))
      sim.setSimulationStep(step)
      sim.updateTasks(tasks)
      sim.updateNorms(norms)
      sim.setAllRoles(allRoles)
      reservedRoles.foreach { case (agent, role) =>
        sim.getReservedRoles() + (agent -> role) // merge
      }
      sim.setReservedRoles(reservedRoles)


      Observation(
        agentId = agentId,
        currentPos = currentPos,
        orientation = orientation,
        energy = energy,
        things = things,
        attached = attached,
        currentRole = currentRole,
        globalMap = globalMap,
        simulation = sim,
        goalZones = goalZones,
        roleZones = roleZones
      )
    }

    def handleActionRequest(json: Json): Json = {
      val cursor = json.hcursor
      val id = cursor.downField("content").get[Int]("id").getOrElse(0)

      val observation = createObservation(json)
      observation.updateKnownMap()

      val action = intentionHandler.planNextAction(observation)
      val actionType = action.actionType
      val params = action.params.map(Json.fromString)

      // Update global map from observation
      globalMap = observation.globalMap
//      println(agentName + " Map: " + globalMap)

      Json.obj(
        "type" -> Json.fromString("action"),
        "content" -> Json.obj(
          "id" -> Json.fromInt(id),
          "type" -> Json.fromString(actionType),
          "p" -> Json.fromValues(params)
        )
      )
    }



    //    def handleActionRequest(json: Json): Json = {
//      val cursor = json.hcursor
//      val id = cursor.downField("content").get[Int]("id").getOrElse(0)
//
//      val observation = createObservation(json)
//      observation.updateKnownMap()
//
//      val action = intentionHandler.planNextAction(observation)
//      val actionType = action.actionType
//
//      //Update global map
//      globalMap = observation.globalMap
//      println(agentName + " Map: " + globalMap)
//
////      val actionType = decideAction(cursor)
//
//      val params = actionType match {
//        case "move" =>
//          val things = getThings(cursor)
//          val blockedDirs = getBlockedDirections(things)
//          val directions = List("n", "s", "e", "w").filterNot(blockedDirs.contains)
//          Json.arr(Json.fromString(directions.headOption.getOrElse("n")))
//        case "rotate" =>
//          Json.arr(Json.fromString("cw"))
//
//        case "attach" =>
//          val things = getThings(cursor)
//          val direction = findThingDirection(things, "block")
//          Json.arr(Json.fromString(direction.getOrElse("n")))
//
//        case "detach" =>
//          Json.arr(Json.fromString("n"))
//
//        case "adopt" =>
//          val roles = cursor.downField("content").downField("percept").get[Vector[Json]]("roles").getOrElse(Vector())
//          val roleName = roles.headOption.flatMap(_.hcursor.get[String]("name").toOption).getOrElse("worker")
//          Json.arr(Json.fromString(roleName))
//
//        case "clear" =>
//          val things = getThings(cursor)
//          val obstacle = things.find(t => t.hcursor.get[String]("type").getOrElse("") == "obstacle")
//          val (x, y) = getThingCoordinates(obstacle)
//          Json.arr(Json.fromInt(x), Json.fromInt(y))
//
//        case "connect" =>
//          Json.arr(Json.fromString("agentA2"), Json.fromInt(0), Json.fromInt(1)) // Needs real logic
//
//        case "disconnect" =>
//          Json.arr(Json.fromInt(0), Json.fromInt(1), Json.fromInt(1), Json.fromInt(1))
//
//        case "request" =>
//          val things = getThings(cursor)
//          val dispenserDir = findThingDirection(things, "dispenser")
//          Json.arr(Json.fromString(dispenserDir.getOrElse("n")))
//
//        case "submit" =>
//          val tasks = cursor.downField("content").downField("percept").get[Vector[Json]]("tasks").getOrElse(Vector())
//          val taskName = tasks.headOption.flatMap(_.hcursor.get[String]("name").toOption).getOrElse("task0")
//          Json.arr(Json.fromString(taskName))
//
//        case _ => Json.arr()
//      }
//
//      Json.obj(
//        "type" -> Json.fromString("action"),
//        "content" -> Json.obj(
//          "id" -> Json.fromInt(id),
//          "type" -> Json.fromString(actionType),
//          "p" -> params
//        )
//      )
//    }

    def getThings(cursor: HCursor): Vector[Json] = {
      cursor.downField("content").downField("percept").get[Vector[Json]]("things").getOrElse(Vector())
    }

    def parseThings(percept: ACursor): Vector[Thing] = {
      percept.get[Vector[Json]]("things").getOrElse(Vector()).flatMap { thingJson =>
        val cursor = thingJson.hcursor
        for {
          x <- cursor.get[Int]("x").toOption
          y <- cursor.get[Int]("y").toOption
          typ <- cursor.get[String]("type").toOption
          details <- cursor.get[String]("details").toOption
        } yield Thing(x, y, typ, details)
      }
    }


    def getBlockedDirections(things: Vector[Json]): List[String] = {
      things.flatMap { thing =>
        val typ = thing.hcursor.get[String]("type").getOrElse("")
        val (x, y) = getThingCoordinates(Some(thing))
        if (typ == "obstacle" || typ == "entity") directionFromCoordinates(x, y) else None
      }.toList
    }

    def findThingDirection(things: Vector[Json], targetType: String): Option[String] = {
      val thing = things.find(t => t.hcursor.get[String]("type").getOrElse("") == targetType).asJson
      val (x, y) = getThingCoordinates(Some(thing))
      directionFromCoordinates(x, y)
    }



    // Maps (x, y) offsets to direction
    def directionFromCoordinates(x: Int, y: Int): Option[String] = (x, y) match {
      case (0, -1) => Some("n")
      case (0, 1)  => Some("s")
      case (-1, 0) => Some("w")
      case (1, 0)  => Some("e")
      case _      => None
    }

    def getThingCoordinates(thing: Option[Json]): (Int, Int) = {
      val cursor = thing.map(_.hcursor).getOrElse(Json.Null.hcursor)
      val x = cursor.get[Int]("x").getOrElse(0)
      val y = cursor.get[Int]("y").getOrElse(0)
      (x, y)
    }

    def decideAction(cursor: HCursor): String = {
      val thingsCursor = cursor.downField("content").downField("percept").downField("things")
      val things = thingsCursor.as[Vector[Json]].getOrElse(Vector.empty)

      // 1. Try to find an obstacle adjacent to the agent
      val obstacleNearby = things.exists { t =>
        val c = t.hcursor
        val tpe = c.get[String]("type").getOrElse("")
        val x = c.get[Int]("x").getOrElse(99)
        val y = c.get[Int]("y").getOrElse(99)

        tpe == "obstacle" && math.abs(x) + math.abs(y) == 1
      }

      if (obstacleNearby) {
        return "clear"
      }

      // 2. Try to find a free direction to move
      val blockedDirs = things.flatMap { t =>
        val c = t.hcursor
        val tpe = c.get[String]("type").getOrElse("")
        val x = c.get[Int]("x").getOrElse(99)
        val y = c.get[Int]("y").getOrElse(99)

        if (tpe != "entity" && tpe != "obstacle") None
        else {
          (x, y) match {
            case (0, -1) => Some("n")
            case (0, 1)  => Some("s")
            case (1, 0)  => Some("e")
            case (-1, 0) => Some("w")
            case _ => None
          }
        }
      }.toSet

      val allDirs = Seq("n", "s", "e", "w")
      val freeDirs = allDirs.filterNot(blockedDirs.contains)

      freeDirs.headOption.map(_ => "move").getOrElse("skip")
    }

    def rotateDirection(current: String, rotation: String): String = {
      val directions = Seq("n", "e", "s", "w")
      val idx = directions.indexOf(current)
      rotation match {
        case "cw" => directions((idx + 1) % 4)
        case "ccw" => directions((idx + 3) % 4)
        case _ => current
      }
    }


  }





}
