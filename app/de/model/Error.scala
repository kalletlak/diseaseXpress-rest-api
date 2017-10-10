package de.model

import play.api.libs.json.Json
import play.api.libs.json.JsObject
import play.api.libs.json.JsString

trait Error {
  val key: String
  val value: Seq[String]
  //TODO: format depending on error type
  def formatJson(): JsObject = Json.obj(
    "key" -> key,
    "value" -> value)
}

case class GeneIdError(override val value: Seq[String]) extends Error {
  override val key = "gene_id"
}

case class GeneSymbolError(override val value: Seq[String]) extends Error {
  override val key = "gene_symbol"
}

case class TranscriptIdError(override val value: Seq[String]) extends Error {
  override val key = "transcript_id"
}

case class StudyIdError(override val value: Seq[String]) extends Error {
  override val key = "study_id"
}

case class SampleIdError(override val value: Seq[String]) extends Error {
  override val key = "sample_id"
}

case class NormalizationError(override val value: Seq[String]) extends Error {
  override val key = "normalization"
}

case class PorjectionError(override val value: Seq[String]) extends Error {
  override val key = "projection"
}