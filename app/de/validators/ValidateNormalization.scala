package de.validators

import de.utils.Enums.Normalization
import play.api.libs.json.{ JsObject, JsString, Json }
import de.model.Error

object ValidateNormalization {

  def apply(normalizations: Option[String]): Either[Error, Seq[Normalization]] = {
    normalizations match {

      case Some(normalization_str) => {
            val normalization_obj = normalization_str
                                      .split(",", -1)
                                      .map { Normalization.withNameOption }.toSeq
            if (normalization_obj
                  .filter(_.isEmpty)
                  .isEmpty)
              Right(normalization_obj.flatten)
            else
              Left(Error("normalization", normalization_str
                .split(",", -1).filter { x => Normalization.withNameOption(x).isEmpty }))
      }

      case None => Right(
        Seq(Normalization.rsem,
          Normalization.sample_abundance,
          Normalization.sample_rsem_isoform))
    }
  }

}
