package de.validators

import de.utils.Enums.Projection
import play.api.libs.json.{ JsObject, JsString, Json }
import de.model.Error

object ValidateProjection {

  def apply(projection: Option[String]): Either[Error, Projection] =
    projection match {

      case Some(projection) => Projection.withNameOption(projection) match {

        case Some(projection) => Right(projection)
        case None             =>  Left(Error("projection", Seq(projection)))
      }

      case None => Right(Projection.summary)

    }
}
