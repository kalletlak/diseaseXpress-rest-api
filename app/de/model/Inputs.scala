package de.model

// ===========================================================================
object Inputs {

  trait FilterUnit {
        
    val key:    String
    val values: Option[Seq[String]]
    
    // ---------------------------------------------------------------------------
    def queryMongoString: Option[String] =
      values match {
        case Some(_values) => Some(s"""{$key: {$$in: ${seqAsMongoString(_values)}}}""")
        case None          => None
      }

    def queryCassandraString: Option[String] =
      values match {
        case Some(_values) => Some(s"$key in ${_values.mkString("('", "','", "')")}")
        case None          => None
      }

    def queryElasticSearchString: Option[String] =
      values match {
        case Some(_values) => Some(s"""{ "terms" : { "$key" : ${_values.map { value => s""" "$value" """ }.mkString("[", ",", "]")} } }""") // TODO: use JsObject
        case None          => None
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
      override val values: Option[Seq[String]])
      extends FilterUnit {
    
    override val key = "study_id"
    
  }
  
  // ---------------------------------------------------------------------------
  case class SampleFilter(
        override val values: Option[Seq[String]])
      extends FilterUnit {
    
    override val key = "sample_id"
    
  }
  
  // ---------------------------------------------------------------------------
  case class GeneIdFilter(
        override val values: Option[Seq[String]])
      extends FilterUnit {
    
    override val key = "gene_id"
    
  }
  
  // ---------------------------------------------------------------------------
  case class GeneSymbolFilter(
        override val values: Option[Seq[String]])
      extends FilterUnit {
    
    override val key = "gene_symbol"
    
  }
  
  // ---------------------------------------------------------------------------
  case class TranscriptIdFilter(
        override val values: Option[Seq[String]])
      extends FilterUnit {
    
    override val key = "transcript_id"
    
  }

  // ===========================================================================  
  sealed trait GeneQueryRef { val ref_id: Seq[String] }
    case class GeneIdQuery      (override val ref_id: Seq[String]) extends GeneQueryRef
    case class GeneSymbolQuery  (override val ref_id: Seq[String]) extends GeneQueryRef
    case class TranscriptIdQuery(override val ref_id: Seq[String]) extends GeneQueryRef

  case class InputFilters( // TODO: subclass instead
    ref_id:    Option[GeneQueryRef] = None,
    study_id:  Option[Seq[String]]  = None,
    sample_id: Option[Seq[String]]  = None)

  // ===========================================================================  
  trait InputDataModel { val collection_name: String }

  trait TranscriptModel extends InputDataModel

  trait GeneModel extends InputDataModel

  // ===========================================================================  
  case class AbundanceProjectons(
        length:            Boolean = false,
        effective_length:  Boolean = false,
        expected_count:    Boolean = false,
        tpm:               Boolean = true)
      extends TranscriptModel {
    
    val sample_id:       Boolean = true
    val collection_name: String = "transcript_abundance"

  }

  // ===========================================================================
  case class RsemGeneProjectons(
        length:            Boolean = false,
        effective_length:  Boolean = false,
        expected_count:    Boolean = false,
        tpm:               Boolean = false,
        fpkm:              Boolean = true)
      extends TranscriptModel {
    
    val sample_id:       Boolean = true
    val collection_name: String = "gene_rsem"
    
  }

  // ===========================================================================
  case class RsemIsoformProjectons(
        length:             Boolean = false,
        effective_length:   Boolean = false,
        expected_count:     Boolean = false,
        tpm:                Boolean = true,
        fpkm:               Boolean = false,
        isoform_percentage: Boolean = false)
      extends GeneModel {
    
    val sample_id:       Boolean = true
    val collection_name: String  = "transcript_isoform"

  }

}

// ===========================================================================
