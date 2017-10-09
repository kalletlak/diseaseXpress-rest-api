package de.validators

import de.utils.Enums.Projection
import play.api.libs.json.{ JsObject, JsString, Json }
import de.model.input.ErrorMsg

object ValidateProjection {

  def apply(projection: Option[String]): Either[ErrorMsg, Projection] =
    projection match {

      case Some(projection) => Projection.withNameOption(projection) match {

        case Some(projection) => Right(projection)
        case None             =>  Left(ErrorMsg("projection", Seq(projection)))
      }

      case None => Right(Projection.summary)

    }
}
