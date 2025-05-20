package model

import model.RegulationType.RegulationType


case class NormRegulation(
                           subject: String, // e.g., "block", "role"
                           filter: String, // e.g., "any", "default", etc.
                           regQuantity: Int, // the number this regulation constrains
                           regType: String // e.g., BLOCK, ROLE
                         )
