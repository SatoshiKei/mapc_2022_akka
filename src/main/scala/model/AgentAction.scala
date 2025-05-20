package model

case class AgentAction(
                        actionType: String,
                        params: Seq[String] = Seq.empty
                      )