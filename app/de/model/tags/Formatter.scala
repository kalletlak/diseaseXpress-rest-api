package de.model.tags

// ===========================================================================
trait Formatter {
  val formatQuery: String
  val formatJson:  play.api.libs.json.JsValue
}

// ===========================================================================
