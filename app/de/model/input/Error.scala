package de.model.input

import play.api.libs.json.Json

case class ErrorMsg(key: String, value: Seq[String])

object ErrorMsg {
  implicit val readsErrorMsg = Json.reads[ErrorMsg]

  implicit val writesErrorMsg = Json.writes[ErrorMsg]
}
