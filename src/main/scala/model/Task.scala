package model

case class Task(
                 name: String,
                 deadline: Int,
                 reward: Int,
                 requirements: List[TaskRequirement]
               ) {

  override def equals(other: Any): Boolean = other match {
    case that: Task => this.name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    name.hashCode
  }
}
