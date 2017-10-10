package de.model.input

import de.utils.Enums.{ Normalization, Projection }
import de.validators.{ ValidateNormalization, ValidateProjection }
import de.model.output.GeneInfo
import de.controllers.EnumCombo
import de.validators.PrimaryIdsValidator
import de.validators.GeneIdQuery
import de.validators.SecondaryIdsValidator
import de.validators.StudyQuery
import de.model.Error
import de.validators.SecondaryIdRef

case class InputFilters(
  primary_ref_ids: Seq[GeneInfo],
  secondary_ref_ids: SecondaryIdRef,
  normalization_combo:Map[Normalization, InputDataModel])

// ===========================================================================

object InputFilters {

  def apply(
    primaryObject: PrimaryIdsValidator=GeneIdQuery,
    secondaryObject: SecondaryIdsValidator=StudyQuery,
    primaryIds: Option[String] = None,
    secondaryIds: Option[String] = None,
    normalizations: Option[String] = None,
    projection: Option[String] = None): Either[Seq[Error], InputFilters] = {

    val _primary_ids = primaryIds match {
      case Some(_ids) => primaryObject(_ids)
      case _       => Right(Seq())
    }

    val _secondary_ids = secondaryIds match {
      case Some(_ids) => secondaryObject(_ids)
      case _       => Right(StudyQuery(ref_id = Seq()))
    }

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
