package de.model.input

// ===========================================================================
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
