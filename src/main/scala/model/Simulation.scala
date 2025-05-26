package model

import model.NormRequirementType.NormRequirementType

import scala.collection.mutable

class Simulation(val teamName: String) {

//  private var staticPercept: Option[StaticPercept] = None
  private var tasks: Set[Task] = Set.empty
  private var norms: mutable.Map[String, Norm] = mutable.Map.empty
  private var allRoles: Vector[Role] = Vector()

  private var agentMaxEnergy: Int = 0
  private var clearEnergyCost: Option[Double] = None
  private var energyRecharge: Option[Double] = None
  private var simulationStep: Int = 0
  private var mapCount: Option[Int] = None
  private var reservedRoles: Map[String, Role] = Map()

  def setReservedRoles(roles: Map[String, Role]): Unit = {
    reservedRoles = roles
  }

  def setAllRoles(roles: Vector[Role]): Unit = {
    allRoles = roles
  }

  def getMaxAllowedAgentsForRole(roleName: String): Option[Int] = {
    getActiveNormRequirements("role")
      .find(_.name == roleName)
      .map(_.quantity)
  }


  def normHandleInterval: Int = 20

//  def setStaticPercept(wrapper: StaticPercept): Unit = {
//    this.staticPercept = Some(wrapper)
//  }

//  def getStaticPercept: Option[StaticPercept] = staticPercept

  def setSimulationStep(step: Int): Unit = {
    simulationStep = step
  }

  def getSimulationStep: Int = simulationStep

//  def isLastStep: Boolean = staticPercept.exists(_.steps == simulationStep + 1)

//  def lastStep: Int = staticPercept.map(_.steps).getOrElse(0)

  def setAgentMaxEnergy(value: Int): Unit = {
    agentMaxEnergy = Math.max(agentMaxEnergy, value)
  }

  def getAgentMaxEnergy: Int = agentMaxEnergy

  def setAgentEnergyRecharge(value: Double): Unit = {
    energyRecharge = energyRecharge match {
      case Some(current) => Some(Math.max(current, value))
      case None => Some(value)
    }
  }

  def getAgentEnergyRecharge: Double = energyRecharge.getOrElse(1.0)

  def setClearEnergyCost(value: Double): Unit = {
    clearEnergyCost = clearEnergyCost match {
      case Some(current) => Some(Math.min(current, value))
      case None => Some(value)
    }
  }

  def getClearEnergyCost: Double = clearEnergyCost.getOrElse(3.0)

  def updateTasks(newTasks: Seq[Task]): Unit = {
    tasks = newTasks.filter(t => t.deadline > simulationStep).toSet
  }

  def getTasks: Seq[Task] = tasks.toSeq

  def getAllRoles: Vector[Role] = allRoles

  def hasTaskExpired(task: Task): Boolean = !tasks.contains(task)

  def updateNorms(newNorms: Seq[Norm]): Unit = {
    newNorms.foreach { norm =>
      if (!norms.contains(norm.name)) norms(norm.name) = norm
    }
    norms.retain { case (_, norm) => norm.until >= simulationStep }
  }

  def getNorms(needHandled: Boolean, needConsidered: Option[Boolean] = None): Seq[Norm] = {
    norms.values.toSeq.filter { n =>
      n.start <= simulationStep + normHandleInterval &&
        n.handled == needHandled &&
        (needConsidered.isEmpty || n.considered == needConsidered.get)
    }
  }

  def getNormRequirements(normsList: Seq[Norm], regulationType: Option[String] = None): Seq[NormRequirement] = {
    normsList.flatMap(_.requirements).filter(r => regulationType.forall(_ == r.`type`))
  }


  def getActiveNormRequirements(regulationType: String): Seq[NormRequirement] = {
    getNorms(needHandled = true, needConsidered = Some(true))
      .flatMap(_.requirements)
      .filter(_.`type` == regulationType)
  }


  def setMapCount(value: Int): Unit = {
    mapCount = Some(value)
  }

  def getMapCount: Option[Int] = mapCount

  def setNormHandled(normName: String): Unit = {
    norms.get(normName).foreach(_.handled = true)
  }

  def setNormUnconsidered(normName: String): Unit = {
    norms.get(normName).foreach { norm =>
      norm.handled = true
      norm.considered = false
    }
  }

  def hasActiveNormFor(requirementType: String, name: String): Boolean = {
    getActiveNormRequirements(requirementType).exists(_.name == name)
  }

  def getPunishmentForNorm(requirementType: String, name: String): Option[Int] = {
    getNorms(needHandled = true, needConsidered = Some(true))
      .find(_.requirements.exists(r => r.`type` == requirementType && r.name == name))
      .map(_.punishment)
  }

  def getTasksSortedByDeadline: Seq[Task] = tasks.toSeq.sortBy(_.deadline)

  def getTasksSortedByReward: Seq[Task] = tasks.toSeq.sortBy(-_.reward)

  def isNormActive(norm: Norm): Boolean = {
    norm.start <= simulationStep &&
      norm.until >= simulationStep &&
      norm.considered &&
      norm.handled
  }


  def reset(): Unit = {
    tasks = Set.empty
    norms.clear()
    reservedRoles = Map.empty
    simulationStep = 0
  }

  def getMaxBlockRegulation: Option[Int] = {
    getActiveNormRequirements("block").map(_.quantity).filter(_ != 2).sorted.headOption
  }

  def getReservedRoles(): Map[String, Role] = {
    reservedRoles
  }

  //TODO - Consider role count from all agents to determine norm violation
  def getAllowedFallbackRoles(currentRole: Option[String]): Seq[String] = {
    // Get norm-imposed limits on roles
    val roleLimits: Map[String, Int] =
      getActiveNormRequirements("role").map(r => r.name -> r.quantity).toMap

    // Dynamically score roles by usefulness (e.g., number of unique actions, speed, vision)
    def roleScore(role: Role): Int = {
      val actionScore = role.actions.toSeq.distinct.size
      val speedScore = role.speed.headOption.get
      val visionScore = role.vision
      actionScore + speedScore + visionScore
    }

    // Filter out current role, and restrict roles that are explicitly prohibited by norms
    val allowedRoles = allRoles.filter { role =>
      val notCurrent = !currentRole.contains(role.name)
      val isNotBannedByNorm = !roleLimits.contains(role.name) || roleLimits(role.name) > 0
      notCurrent && isNotBannedByNorm
    }

    // Sort the allowed roles by dynamic score (descending)
    allowedRoles.sortBy(role => -roleScore(role)).map(_.name)
  }



}
