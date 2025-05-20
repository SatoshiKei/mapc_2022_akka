package model

case class Task(
                 name: String,
                 deadline: Int,
                 reward: Int,
                 requirements: List[Requirement]
               ) {

  override def equals(other: Any): Boolean = other match {
    case that: Task => this.name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    name.hashCode
  }
}
