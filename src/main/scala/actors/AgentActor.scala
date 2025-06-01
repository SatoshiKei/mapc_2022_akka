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
  import shared.{CoordinateAlignment, KnownAgent, MapMerger, ShareMap, WhoIsHere}

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
    var knownRoleZones: Set[Coordinate] = Set.empty
    var knownGoalZones: Set[Coordinate] = Set.empty
    var orientation: String = "n"
    var currentRole: String = "std"
    var allRoles: Vector[Role] = Vector()
    var team: String = ""
    var teamSize: Int = 0
    var currentStep: Int = 0
    val knownAgents: mutable.Map[String, KnownAgent] = mutable.Map.empty

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
        handleMassimMessage(jsonString)


      case who: WhoIsHere =>
        handleWhoIsHere(who)

      case mapShare: ShareMap =>
        handleMapShare(mapShare)

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

    def handleMassimMessage(jsonString: String) = {
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
              team = json.hcursor.downField("content").downField("percept").get[String]("team").getOrElse("")
              teamSize = json.hcursor.downField("content").downField("percept").get[Int]("teamSize").getOrElse(0)
              println("🚀 Simulation started. Team " + team + ":" + teamSize)
            //                println("Roles: " + roles + allRoles)
            case "request-action" =>

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

              println(s"Last Action: $agentName/$energy/$currentRole $globalPosition  [$lastAction/$lastActionParams -> $lastActionResult]")
              val action = handleActionRequest(json)
              println(agentName + " is sending action: " + action.noSpaces)
              sendMessage(action)
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

    def broadcastWhoIsHere(step: Int, observation: Observation): Unit = {
      val message = WhoIsHere(
        senderName = agentName,
        senderStep = step,
        senderGlobalPos = globalPosition,
        senderPercept = observation.things
      )
      for (i <- 1 to teamSize if s"agent$team$i" != agentName) {
        context.actorSelection(s"/user/agent$team$i") ! message
      }
    }

    def handleWhoIsHere(msg: WhoIsHere): Unit = {
      if (msg.senderStep != currentStep) return

      val maybeOffset = CoordinateAlignment.findOffset(observation.get.things, msg.senderPercept)
      maybeOffset.foreach { offset =>
        knownAgents(msg.senderName) = KnownAgent(msg.senderName, offset, currentStep)
        val sharedData = ShareMap(
          senderName = agentName,
          senderStep = currentStep,
          translatedMap = globalMap.filterNot { case (k, _) => globalMap.contains(k) }
        )
        context.actorSelection(s"/user/${msg.senderName}") ! sharedData
      }
    }

    def handleMapShare(msg: ShareMap): Unit = {
      knownAgents.get(msg.senderName).foreach { known =>
        val transformedMap = msg.translatedMap.map { case (coord, typ) =>
          (coord + known.offset) -> typ
        }
        MapMerger.merge(globalMap, transformedMap)
      }
    }

//    def handleActionRequest(json: Json) = {
//      val observation = createObservation(json)
//      this.lastObservation = observation
//      val step = observation.simulation.getSimulationStep
//      observation.updateKnownMap()
//      broadcastWhoIsHere(step, observation)
//      // ... rest of planning ...
//    }

    def createObservation(json: Json): Observation = {
      val cursor = json.hcursor
      val content = cursor.downField("content")
      val percept = content.downField("percept")

      // Agent info
      val agentId = agentName


      val currentPos = globalPosition
//      println(agentName + " position: " + currentPos)


      val energy = percept.get[Int]("energy").getOrElse(0)


      val things = parseThings(percept)
//      println(agentName + "'s things: " + things)


      val attached = percept.get[Vector[(Int, Int)]]("attached").getOrElse(Vector()).map { case (x, y) => Coordinate(x, y) }


      val step = content.get[Int]("step").getOrElse(0)
      this.currentStep = step

      val tasks = percept.get[Vector[Task]]("tasks").getOrElse(Vector())
      println("Tasks: " + tasks)


      val currentRole = percept.get[String]("role").toOption
      this.currentRole = currentRole.get


      val norms = percept.get[Vector[Norm]]("norms").getOrElse(Vector())


      val reservedRoles: Map[String, Role] =
        percept.get[Vector[Json]]("reservedRoles").getOrElse(Vector()).flatMap { roleJson =>
          for {
            agent <- roleJson.hcursor.get[String]("agent").toOption
            role <- roleJson.hcursor.get[Role]("role").toOption
          } yield agent -> role
        }.toMap


      val goalZones: Set[Coordinate] =
        percept.get[Vector[(Int, Int)]]("goalZones")
          .getOrElse(Vector())
          .map { case (x, y) =>
            globalPosition + Coordinate(x, y)
          }
          .toSet


      val roleZones: Set[Coordinate] =
        percept.get[Vector[(Int, Int)]]("roleZones")
          .getOrElse(Vector())
          .map { case (x, y) =>
            globalPosition + Coordinate(x, y)
          }
          .toSet
//      println("Current: " + globalPosition + "Percept: " + percept.get[Vector[(Int, Int)]]("roleZones") + " roleZones: " + roleZones)


      goalZones.foreach { coord =>
        knownGoalZones += coord
      }

      roleZones.foreach { coord =>
        knownRoleZones += coord
      }


      val sim = new Simulation(teamName = percept.get[String]("team").getOrElse("A"))
      sim.setSimulationStep(step)
      sim.updateTasks(tasks)
      sim.updateNorms(norms)
      sim.setAllRoles(allRoles)
      reservedRoles.foreach { case (agent, role) =>
        sim.getReservedRoles() + (agent -> role) // merge
      }
      sim.setReservedRoles(reservedRoles)
      sim.setTeam(team)
      sim.setTeamSize(teamSize)

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
        roleZones = roleZones,
        knownGoalZones = knownGoalZones,
        knownRoleZones = knownRoleZones
      )
    }

    def handleActionRequest(json: Json): Json = {
      val cursor = json.hcursor
      val id = cursor.downField("content").get[Int]("id").getOrElse(0)

      val observation = createObservation(json)
      observation.updateKnownMap()
      observation.printKnownDispenserSummary()
      this.observation = Some(observation)

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
