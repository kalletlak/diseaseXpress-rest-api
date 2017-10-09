package de.model.input

// ===========================================================================
trait FilterUnit {

  val key: String
  val values: Seq[String]

  // ---------------------------------------------------------------------------
  def queryMongoString: Option[String] =
    values.isEmpty match {
      case false => Some(s"""{$key: {$$in: ${seqAsMongoString(values)}}}""")
      case true  => None
    }

  def queryCassandraString: Option[String] =
    values.isEmpty match {
      case false => Some(s"$key in ${values.mkString("('", "','", "')")}")
      case true  => None
    }

  def queryElasticSearchString: Option[String] =
    values.isEmpty match {
      case false => Some(s"""{ "terms" : { "$key" : ${values.map { value => s""" "$value" """ }.mkString("[", ",", "]")} } }""") // TODO: use JsObject
      case true  => None
    }

  // ---------------------------------------------------------------------------
  private def seqAsMongoString(values: Seq[String]) =
    values
      .map(stringWithDoubleQuotes)
      .mkString("[", ", ", "]")

  private def stringWithDoubleQuotes(str: String) =
    s""""$str""""

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
