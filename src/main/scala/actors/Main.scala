import actors.{AgentActor, MassimAgent}
import akka.actor.{ActorSystem, Props}

object Main extends App {

  val system = ActorSystem("MassimSystem")
//  val agent1 = system.actorOf(AgentActor.props(), "agentA1")

  val teamA = "A"
  val teamB = "B"
  val password = "1"
  val numberOfAgents = 5

  for (i <- 1 until numberOfAgents+1) {
    val agentNameA = s"agent${teamA}${i}"
    val agentNameB = s"agent${teamB}${i}"
//    system.actorOf(Props(new MassimAgent(agentNameA, password)), name = agentNameA)
//    system.actorOf(Props(new MassimAgent(agentNameB, password)), name = agentNameB)
    system.actorOf(Props(new AgentActor(agentNameA, password)), name = agentNameA)
    system.actorOf(Props(new AgentActor(agentNameB, password)), name = agentNameB)

  }
}
