package de.model

import play.api.libs.json.Json

case class Error(key: String, value: Seq[String])

object Error {
  implicit val readsError = Json.reads[Error]

  implicit val writesError = Json.writes[Error]
}
