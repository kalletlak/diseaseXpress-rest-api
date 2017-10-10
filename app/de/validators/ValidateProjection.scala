package de.validators

import de.model.{ Error, PorjectionError }
import de.utils.Enums.Projection

object ValidateProjection {

  def apply(projection: Option[String]): Either[Error, Projection] =
    projection match {

      case Some(projection) => Projection.withNameOption(projection) match {

        case Some(projection) => Right(projection)
        case None             =>  Left(PorjectionError(Seq(projection)))
      }

      case None => Right(Projection.summary)

    }
}
