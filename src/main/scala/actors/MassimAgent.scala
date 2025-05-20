package actors {

  import akka.actor.Actor
  import akka.actor.Props
  import java.io._
  import java.net.Socket

  import io.circe._, io.circe.parser._, io.circe.syntax._

  object MassimAgent {
    def props(agentName: String, password: String): Props = Props(new MassimAgent(agentName: String, password: String))
  }

  class MassimAgent(agentName: String, password: String) extends Actor {

    val socket = new Socket("localhost", 12300)
    val writer = new PrintWriter(socket.getOutputStream, true)
    val reader = new BufferedReader(new InputStreamReader(socket.getInputStream))

    override def preStart: Unit = {
      println("Connecting to MASSim...")

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
                println("🚀 Simulation started.")

              case "request-action" =>
                val action = handleActionRequest(json)
                sendMessage(action)
                println("Sending action: " + action.noSpaces)

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

    def handleActionRequest(json: Json): Json = {
      val cursor = json.hcursor
      val id = cursor.downField("content").get[Int]("id").getOrElse(0)
      val actionType = decideAction(cursor)

      val params = actionType match {
        case "move" =>
          val things = getThings(cursor)
          val blockedDirs = getBlockedDirections(things)
          val directions = List("n", "s", "e", "w").filterNot(blockedDirs.contains)
          Json.arr(Json.fromString(directions.headOption.getOrElse("n")))

        case "rotate" =>
          Json.arr(Json.fromString("cw"))

        case "attach" =>
          val things = getThings(cursor)
          val direction = findThingDirection(things, "block")
          Json.arr(Json.fromString(direction.getOrElse("n")))

        case "detach" =>
          Json.arr(Json.fromString("n"))

        case "adopt" =>
          val roles = cursor.downField("content").downField("percept").get[Vector[Json]]("roles").getOrElse(Vector())
          val roleName = roles.headOption.flatMap(_.hcursor.get[String]("name").toOption).getOrElse("worker")
          Json.arr(Json.fromString(roleName))

        case "clear" =>
          val things = getThings(cursor)
          val obstacle = things.find(t => t.hcursor.get[String]("type").getOrElse("") == "obstacle")
          val (x, y) = getThingCoordinates(obstacle)
          Json.arr(Json.fromInt(x), Json.fromInt(y))

        case "connect" =>
          Json.arr(Json.fromString("agentA2"), Json.fromInt(0), Json.fromInt(1)) // Needs real logic

        case "disconnect" =>
          Json.arr(Json.fromInt(0), Json.fromInt(1), Json.fromInt(1), Json.fromInt(1))

        case "request" =>
          val things = getThings(cursor)
          val dispenserDir = findThingDirection(things, "dispenser")
          Json.arr(Json.fromString(dispenserDir.getOrElse("n")))

        case "submit" =>
          val tasks = cursor.downField("content").downField("percept").get[Vector[Json]]("tasks").getOrElse(Vector())
          val taskName = tasks.headOption.flatMap(_.hcursor.get[String]("name").toOption).getOrElse("task0")
          Json.arr(Json.fromString(taskName))

        case _ => Json.arr()
      }

      Json.obj(
        "type" -> Json.fromString("action"),
        "content" -> Json.obj(
          "id" -> Json.fromInt(id),
          "type" -> Json.fromString(actionType),
          "p" -> params
        )
      )
    }

    def getThings(cursor: HCursor): Vector[Json] = {
      cursor.downField("content").downField("percept").get[Vector[Json]]("things").getOrElse(Vector())
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



  }






}
