package de.model.input

import play.api.libs.json.{ JsValue, Json }
import de.model.Fields

// ===========================================================================
trait FilterUnit {

  val key: String
  val values: Seq[String]

  // ---------------------------------------------------------------------------
  def queryMongoString: JsValue =
    Json.obj(key -> Json.obj(Fields.$in.entryName -> values))

  def queryCassandraString: String =
    s"$key in ${values.mkString("('", "','", "')")}"

  def queryElasticSearchString: JsValue =
    Json.obj(Fields.terms.entryName -> Json.obj(key -> values ) )
}

// ===========================================================================  
case class StudyFilter(
  override val values: Seq[String])
    extends FilterUnit {

  override val key = "study_id"

}

// ---------------------------------------------------------------------------
case class SampleFilter(
  override val values: Seq[String])
    extends FilterUnit {

  override val key = "sample_id"

}

// ---------------------------------------------------------------------------
case class GeneIdFilter(
  override val values: Seq[String])
    extends FilterUnit {

  override val key = "gene_id"

}

// ---------------------------------------------------------------------------
case class GeneSymbolFilter(
  override val values: Seq[String])
    extends FilterUnit {

  override val key = "gene_symbol"

}

// ---------------------------------------------------------------------------
case class TranscriptIdFilter(
  override val values: Seq[String])
    extends FilterUnit {

  override val key = "transcript_id"

}
    
// ===========================================================================
