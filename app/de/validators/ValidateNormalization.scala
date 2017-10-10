package de.validators

import de.model.{ Error, NormalizationError }
import de.utils.Enums.Normalization

object ValidateNormalization {

  private def getAllNormalization = Normalization.values.map { _.entryName }.toSeq

  def apply(normalizations_str: Option[String]): Either[Error, Seq[Normalization]] = {
    
    val normalizations =  normalizations_str match {
      case Some(_normalizations) => _normalizations.split(",", -1).toSeq
      case None    => getAllNormalization
    }
      
    val invalid_normalizations = 
      normalizations
        .filter { normalization => Normalization
                                      .withNameOption(normalization)
                                      .isEmpty }
    
    if (invalid_normalizations.isEmpty)
      Right(normalizations.map { Normalization.withName })
    else
      Left(NormalizationError(invalid_normalizations))
      
  }
}
