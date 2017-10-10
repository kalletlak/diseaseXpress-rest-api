package de.model.input

import de.utils.Enums.{ Normalization, Projection }
import de.validators.{ ValidateNormalization, ValidateProjection }
import de.model.output.GeneInfo
import de.controllers.EnumCombo
import de.validators.PrimaryIdsValidator
import de.validators.SecondaryIdsValidator
import de.model.Error
import de.validators.SecondaryIdRef
import de.validators.GeneIdFilters
import de.validators.StudyIdFilters

case class InputFilters(
  primary_ref_ids: Seq[GeneInfo],
  secondary_ref_ids: SecondaryIdRef,
  normalization_combo:Map[Normalization, InputDataModel])

// ===========================================================================

object InputFilters {

  def apply(
    primaryObject: PrimaryIdsValidator=GeneIdFilters,
    secondaryObject: SecondaryIdsValidator=StudyIdFilters,
    primaryIds: Seq[String],
    secondaryIds: Seq[String],
    normalizations: Option[String] = None,
    projection: Option[String] = None): Either[Seq[Error], InputFilters] = {

    val _primary_ids = primaryObject(primaryIds)

    val _secondary_ids = secondaryObject(secondaryIds)

    val _normalizations = ValidateNormalization(normalizations)

    val _projection = ValidateProjection(projection)

    val errors = Seq(
                      _primary_ids.left.toOption,
                      _secondary_ids.left.toOption,
                      _normalizations.left.toOption,
                      _projection.left.toOption).flatten

    if (errors.isEmpty) {
      Right(InputFilters(
        primary_ref_ids = _primary_ids.right.get,
        secondary_ref_ids = _secondary_ids.right.get,
        normalization_combo = EnumCombo(
                                        _projection.right.get,
                                        _normalizations.right.get).toMap))
    } else {
      Left(errors)

    }

  }
}
