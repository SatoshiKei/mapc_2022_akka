package model

import model.NormRequirementType.NormRequirementType

case class NormRequirement(
                            `type`: String,
                            name: String,
                            quantity: Int
                     )

