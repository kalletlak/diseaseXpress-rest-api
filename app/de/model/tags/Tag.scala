package de.model.tags

import de.model.tags.Sample.Value
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Writes

// ===========================================================================
case class Tag(
    key:   String,
    value: Value)
    
  // ---------------------------------------------------------------------------
  object Tag {
    implicit val tagWrites =
      new Writes[Tag] {
      
        def writes(tag: Tag) =
          Json.obj(
            "key"   -> tag.key,
            "value" -> tag.value.formatJson)

      }
  }

// ===========================================================================
