package model

import model.RegulationType.RegulationType

import scala.collection.mutable

class Simulation(val teamName: String) {

//  private var staticPercept: Option[StaticPercept] = None
  private var tasks: Set[Task] = Set.empty
  private var norms: mutable.Map[String, Norm] = mutable.Map.empty

  private var agentMaxEnergy: Int = 0
  private var clearEnergyCost: Option[Double] = None
  private var energyRecharge: Option[Double] = None
  private var simulationStep: Int = 0
  private var mapCount: Option[Int] = None
  private var reservedRoles: Map[String, Role] = Map()

  def setReservedRoles(roles: Map[String, Role]): Unit = {
    reservedRoles = roles
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

  def hasTaskExpired(task: Task): Boolean = !tasks.contains(task)

  def updateNorms(newNorms: Seq[Norm]): Unit = {
    newNorms.foreach { norm =>
      if (!norms.contains(norm.name)) norms(norm.name) = norm
    }
    norms.retain { case (_, norm) => norm.untilStep >= simulationStep }
  }

  def getNorms(needHandled: Boolean, needConsidered: Option[Boolean] = None): Seq[Norm] = {
    norms.values.toSeq.filter { n =>
      n.startStep <= simulationStep + normHandleInterval &&
        n.handled == needHandled &&
        (needConsidered.isEmpty || n.considered == needConsidered.get)
    }
  }

  def getNormsRegulations(normsList: Seq[Norm], regulationType: Option[RegulationType] = None): Seq[NormRegulation] = {
    normsList.flatMap(_.regulations).filter(r => regulationType.forall(_ == r.regType))
  }

  def getActiveRegulations(regulationType: RegulationType): Seq[NormRegulation] = {
    getNorms(needHandled = true, needConsidered = Some(true))
      .flatMap(_.regulations)
      .filter(_.regType == regulationType)
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

  def getMaxBlockRegulation: Option[Int] = {
    getActiveRegulations(RegulationType.BLOCK).map(_.regQuantity).filter(_ != 2).sorted.headOption
  }

  def getReservedRoles(): Map[String, Role] = {
    reservedRoles
  }
}
