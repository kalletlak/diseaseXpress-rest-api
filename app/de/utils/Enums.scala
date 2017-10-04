package de.utils

import enumeratum.Enum
import enumeratum.EnumEntry

object Enums {

  sealed trait Projection extends EnumEntry

  object Projection extends Enum[Projection] {
    val values = findValues

    case object summary extends Projection

    case object detailed extends Projection

  }

  sealed trait Normalization extends EnumEntry

  object Normalization extends Enum[Normalization] {
    val values = findValues

    case object rsem extends Normalization

    case object sample_abundance extends Normalization

    case object sample_rsem_isoform extends Normalization

  }

}
