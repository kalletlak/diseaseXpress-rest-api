package de.utils

import play.api.libs.json.JsObject
import play.api.libs.json.JsArray
import play.api.libs.json.JsValue
import play.api.libs.json.JsPath
import play.api.libs.json.{ Json => PlayJson }
import play.api.libs.json.Json
import play.api.libs.json.JsValue.jsValueToJsLookup

/**
 * from Nebulis
 */
object JsObjectWithOption {

  def apply(data: Tuple2[String, Either[JsValue, Option[JsValue]]]*): JsValue =
    JsObject(
      data
        .flatMap {
          case (key, value) =>
            value match {
              case Left(value)   => Some(key -> value)
              case Right(option) => option.map(key -> _)
            }
        })

}

object PlayJsonUtils {

  implicit class JsObjectImplicits(json: JsObject) {
    def parseDoubleOption(fieldName: String,
                          projectionOption: Boolean): Option[Double] =

      if (projectionOption) Some(PlayJsonUtils.parseDoubleOption(json,
        fieldName)
        .getOrElse(0))
      else None

    def parseObjectArray(fieldName: String): Seq[JsObject] =
      PlayJsonUtils.parseObjectArray(json, fieldName)
  }

  def parseDoubleOption(json: JsObject,
                        fieldName: String): Option[Double] =
    parseOption(json, fieldName).map(_.as[Double])

  def parseObjectArray(json: JsObject,
                       fieldName: String): Seq[JsObject] =
    parseArray(json, fieldName).map(_.as[JsObject])

  def parseArray(json: JsObject,
                 fieldName: String): Seq[JsValue] =
    (json \ fieldName).as[JsArray].value.toVector

  def parseOption(json: JsObject,
                  fieldName: String): Option[JsValue] =
    Json.fromJson(json)((JsPath \ fieldName).readNullable[JsValue]).get
}

object NumberUtils {
  implicit class DoubleImplicits(value: Double) {
    def parseDoubleOption(projectionOption: Boolean): Option[Double] =

      if (projectionOption) Some(value)
      else None
  }

}
