package actors {

  import akka.actor.Actor
  import akka.actor.Props

  import java.io._
  import java.net.Socket
  import io.circe._
  import io.circe.parser._
  import io.circe.syntax._
  import io.circe.generic.auto._
  import model.{AgentAction, Coordinate, Observation, Role, TaskTeamRegistry, Thing, Zone, Task}
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
    var globalPosition: Coordinate = Coordinate(0, 0)
    var globalMap: mutable.Map[Coordinate, Thing] = mutable.Map.empty
    var knownGoalZones: mutable.Map[Coordinate, Zone] = mutable.Map.empty
    var knownRoleZones: mutable.Map[Coordinate, Zone] = mutable.Map.empty
    var attached: Vector[Coordinate] = Vector()
    var currentRole: String = "std"
    var allRoles: Vector[Role] = Vector()
    var team: String = ""
    var teamSize: Int = 0
    var currentStep: Int = 0
    val knownAgents: mutable.Map[String, KnownAgent] = mutable.Map.empty
    var knownTasks: mutable.Map[String, Task] = mutable.Map.empty
    var taskRegistry: mutable.Map[String, TaskTeamRegistry] = mutable.Map.empty

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

      case taskRegistry: TaskTeamRegistry =>
        handleTaskTeamRegistryUpdate(taskRegistry)

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
                globalPosition = globalPosition + Coordinate.fromDirection(lastActionParams)
              }

              if (lastAction == "attach" && lastActionResult == "success") {
                val rel = Coordinate.fromDirection(lastActionParams)
                attached = rel +: attached
              }

              if (lastAction == "detach" && lastActionResult == "success") {
                val rel = Coordinate.fromDirection(lastActionParams)
                attached = attached.filterNot(_ == rel)
              }

              if (lastAction == "rotate" && lastActionResult == "success") {
                val newAttachments = attached.map(_.rotateCoordinate(lastActionParams))
                attached = newAttachments
              }


              println(s"Last Action: $agentName/$energy/$currentRole $globalPosition  [$lastAction/$lastActionParams -> $lastActionResult]")
              val action = handleActionRequest(json)

              for ((_, registry) <- taskRegistry) {
                broadcastTaskStatusUpdate(registry)
              }

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

    def broadcastMapShare(): Unit = {
      for ((agentName, known) <- knownAgents) {
        val offset = known.offset

        // Step 1: Translate your map into *their* coordinate system
        val translatedMap: Map[Coordinate, Thing] = globalMap.iterator.map {
          case (coord, value) => (coord - offset) -> value
        }.toMap

        // Step 2: Translate goal zones
        val translatedGoalZones: Map[Coordinate, Zone] = knownGoalZones.iterator.map {
          case (coord, zone) => (coord - offset) -> zone
        }.toMap

        // Step 3: Translate role zones
        val translatedRoleZones: Map[Coordinate, Zone] = knownRoleZones.iterator.map {
          case (coord, zone) => (coord - offset) -> zone
        }.toMap

        // Step 4: Translate known agents' offsets into receiver's coordinate system
        val translatedKnownAgents: Map[String, KnownAgent] = knownAgents.map {
          case (name, agent) =>
            val translatedOffset = agent.offset - offset
            name -> agent.copy(offset = translatedOffset)
        }.toMap

        // Step 5: Construct and send the ShareMap message
        val shareMessage = ShareMap(
          senderName = this.agentName,
          senderStep = currentStep,
          translatedMap = translatedMap,
          translatedGoalZones = translatedGoalZones,
          translatedRoleZones = translatedRoleZones,
          translatedKnownAgents = translatedKnownAgents
        )

        // Step 6: Send to the agent via actor selection
        context.actorSelection(s"/user/$agentName") ! shareMessage
      }
    }


    def handleWhoIsHere(msg: WhoIsHere): Unit = {
      if (msg.senderStep != currentStep) {
        println(agentName + " received an old message from " + msg.senderName + " at step " + msg.senderStep + " while the current step is " + currentStep)
        return
      }

      observation match {
        case Some(observation) =>
          if (observation.things.count(t => t.`type` == "entity") == 1) {
            //println(agentName + " is not seeing any agent at the moment")
            return
          }
        case None =>
          return
      }

      //println(agentName + " received a message WhoIsHere from " + msg.senderName)
      val maybeOffset = CoordinateAlignment.findOffset(observation.get.things, msg.senderPercept)

      maybeOffset.foreach { relOffset =>
        val absOffset = Coordinate(
          globalPosition.x - msg.senderGlobalPos.x + relOffset.x,
          globalPosition.y - msg.senderGlobalPos.y + relOffset.y
        )

        // Store the agent’s alignment offset
        if (!knownAgents.contains(msg.senderName)) {
          knownAgents(msg.senderName) = KnownAgent(
            name = msg.senderName,
            offset = absOffset,
            lastSeenStep = currentStep
          )
          //println(agentName + " stores " + msg.senderName + " as a known agent at step " + currentStep + " with offset " + absOffset)
        }
      }
    }

    def handleMapShare2(msg: ShareMap): Unit = {
      knownAgents.get(msg.senderName).foreach { known =>
        val transformedMap = msg.translatedMap.map { case (coord, typ) =>
          (coord + known.offset) -> typ
        }
        val totalUpdates = MapMerger.merge(globalMap, transformedMap)
        //TODO - After ensuring messages are coming fine, log only successfull map updates. (i.e, totalUpdates > 0)
        //println(agentName + " is synching global map with " + msg.senderName + " by adding " + totalUpdates + " new entries")
      }
    }

    def handleMapShare(msg: ShareMap): Unit = {
      knownAgents.get(msg.senderName).foreach { known =>

        // 1. Merge global map
        val totalMapUpdates = MapMerger.merge(globalMap, msg.translatedMap)

        // 2. Merge goal zones
        val updatedGoals = MapMerger.mergeZones(knownGoalZones, msg.translatedGoalZones)

        // 3. Merge role zones
        val updatedRoles = MapMerger.mergeZones(knownRoleZones, msg.translatedRoleZones)

        // 👥 4. Merge known agents
        msg.translatedKnownAgents.foreach {
          case (otherName, sharedAgent) =>
            if (otherName != agentName) {
              if (!knownAgents.contains(otherName)) {
                knownAgents.update(otherName, KnownAgent(otherName, sharedAgent.offset, sharedAgent.lastSeenStep))
              }
            }
        }

//        if (totalMapUpdates > 0 || updatedGoals > 0 || updatedRoles > 0) {
//          println(s"$agentName synced map from ${msg.senderName}: $totalMapUpdates tiles, $updatedGoals goal zones, $updatedRoles role zones")
//        }
      }
    }

    def handleTaskTeamRegistryUpdate(remote: TaskTeamRegistry): Unit = {


      val taskId = remote.taskId
      val localOpt = taskRegistry.get(taskId)
      val totalRequired = knownTasks.get(remote.taskId).map(_.requirements.size).getOrElse(0)
      println(agentName + " received a task status update for " + remote.taskId + " with " + totalRequired + " requirements at step " + this.currentStep)


      val merged = localOpt match {
        case Some(local) => local.mergeWith(remote, totalRequired)
        case None => remote
      }
      println(agentName + " merge result: " + merged)
      taskRegistry.update(taskId, merged)
    }

    def broadcastTaskStatusUpdate(registry: TaskTeamRegistry): Unit = {
      val totalRequired = knownTasks.get(registry.taskId).map(_.requirements.size).getOrElse(0)

      val allAgentsToInform =
        if (shouldBroadcastToAll(registry, totalRequired)) {
          knownAgents.values.map(agent => agent.name).toSet
        } else {
          registry.assemblies.flatMap(a => a.supporters ).toSet
        }
      println(agentName + " is broadcasting to " + allAgentsToInform + " | " + registry + " | " + knownAgents + " | " + shouldBroadcastToAll(registry, totalRequired))
      allAgentsToInform.foreach { agentId =>
        context.actorSelection(s"/user/$agentId") ! registry
      }
    }

    def shouldBroadcastToAll(registry: TaskTeamRegistry, totalRequired: Int): Boolean = {
      registry.assemblies.exists { a =>
        a.allAgents.size < totalRequired
      }
    }


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


//      val attached = percept.get[Vector[(Int, Int)]]("attached").getOrElse(Vector()).map { case (x, y) => Coordinate(x, y) }


      val step = content.get[Int]("step").getOrElse(0)
      this.currentStep = step

      val tasks = percept.get[Vector[Task]]("tasks").getOrElse(Vector())
      tasks.foreach(t => {
        knownTasks.update(t.name, t)
      })
//      println("Tasks: " + tasks)


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
        knownGoalZones.update(coord, Zone(currentStep, active = true))
      }

      roleZones.foreach { coord =>
        knownRoleZones.update(coord, Zone(currentStep, active = true))
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
        energy = energy,
        things = things,
        attached = attached,
        currentRole = currentRole,
        globalMap = globalMap,
        simulation = sim,
        goalZones = goalZones,
        knownGoalZones = knownGoalZones,
        knownRoleZones = knownRoleZones,
        knownAgents = knownAgents
      )
    }

    def handleActionRequest(json: Json): Json = {
      val cursor = json.hcursor
      val id = cursor.downField("content").get[Int]("id").getOrElse(0)

      val observation = createObservation(json)
      observation.updateKnownMap()
      this.observation = Some(observation)

      if (observation.things.count(t => t.`type` == "entity") > 1) {
        broadcastWhoIsHere(currentStep, observation)
      }
      broadcastMapShare()

      val action = intentionHandler.planNextAction(observation)
      val actionType = action.actionType
      val params = action.params.map(Json.fromString)

      // Update global map from observation
      globalMap = observation.globalMap
      taskRegistry = observation.taskRegistry
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
        } yield Thing(x, y, typ, details, currentStep)
      }
    }


//    def getBlockedDirections(things: Vector[Json]): List[String] = {
//      things.flatMap { thing =>
//        val typ = thing.hcursor.get[String]("type").getOrElse("")
//        val (x, y) = getThingCoordinates(Some(thing))
//        if (typ == "obstacle" || typ == "entity") directionFromCoordinates(x, y) else None
//      }.toList
//    }

//    def findThingDirection(things: Vector[Json], targetType: String): Option[String] = {
//      val thing = things.find(t => t.hcursor.get[String]("type").getOrElse("") == targetType).asJson
//      val (x, y) = getThingCoordinates(Some(thing))
//      directionFromCoordinates(x, y)
//    }



//    def directionFromCoordinates(x: Int, y: Int): Option[String] = (x, y) match {
//      case (0, -1) => Some("n")
//      case (0, 1)  => Some("s")
//      case (-1, 0) => Some("w")
//      case (1, 0)  => Some("e")
//      case _      => None
//    }

//    def getThingCoordinates(thing: Option[Json]): (Int, Int) = {
//      val cursor = thing.map(_.hcursor).getOrElse(Json.Null.hcursor)
//      val x = cursor.get[Int]("x").getOrElse(0)
//      val y = cursor.get[Int]("y").getOrElse(0)
//      (x, y)
//    }

//    def decideAction(cursor: HCursor): String = {
//      val thingsCursor = cursor.downField("content").downField("percept").downField("things")
//      val things = thingsCursor.as[Vector[Json]].getOrElse(Vector.empty)
//
//      // 1. Try to find an obstacle adjacent to the agent
//      val obstacleNearby = things.exists { t =>
//        val c = t.hcursor
//        val tpe = c.get[String]("type").getOrElse("")
//        val x = c.get[Int]("x").getOrElse(99)
//        val y = c.get[Int]("y").getOrElse(99)
//
//        tpe == "obstacle" && math.abs(x) + math.abs(y) == 1
//      }
//
//      if (obstacleNearby) {
//        return "clear"
//      }
//
//      // 2. Try to find a free direction to move
//      val blockedDirs = things.flatMap { t =>
//        val c = t.hcursor
//        val tpe = c.get[String]("type").getOrElse("")
//        val x = c.get[Int]("x").getOrElse(99)
//        val y = c.get[Int]("y").getOrElse(99)
//
//        if (tpe != "entity" && tpe != "obstacle") None
//        else {
//          (x, y) match {
//            case (0, -1) => Some("n")
//            case (0, 1)  => Some("s")
//            case (1, 0)  => Some("e")
//            case (-1, 0) => Some("w")
//            case _ => None
//          }
//        }
//      }.toSet
//
//      val allDirs = Seq("n", "s", "e", "w")
//      val freeDirs = allDirs.filterNot(blockedDirs.contains)
//
//      freeDirs.headOption.map(_ => "move").getOrElse("skip")
//    }
//
//    def rotateDirection(current: String, rotation: String): String = {
//      val directions = Seq("n", "e", "s", "w")
//      val idx = directions.indexOf(current)
//      rotation match {
//        case "cw" => directions((idx + 1) % 4)
//        case "ccw" => directions((idx + 3) % 4)
//        case _ => current
//      }
//    }


  }





}
