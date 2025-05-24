package model

case class Norm(
                 name: String,
                 start: Int,
                 until: Int,
                 level: String,
                 requirements: Seq[NormRequirement],
                 punishment: Int,
                 var handled: Boolean = false,
                 var considered: Boolean = true
               ) {

  override def equals(other: Any): Boolean = other match {
    case that: Norm => this.name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    name.hashCode
  }
}
