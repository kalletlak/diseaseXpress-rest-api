package de.model.input

// ===========================================================================  
sealed trait InputDataModel { val collection_name: String }

  // ---------------------------------------------------------------------------
  sealed trait GeneModel       extends InputDataModel
  
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
  
  // ---------------------------------------------------------------------------
  sealed trait TranscriptModel extends InputDataModel
  
    case class AbundanceProjectons(
          length:            Boolean = false,
          effective_length:  Boolean = false,
          expected_count:    Boolean = false,
          tpm:               Boolean = true)
        extends TranscriptModel {
      
      val sample_id:       Boolean = true
      val collection_name: String = "transcript_abundance"
  
    }
  
    // ---------------------------------------------------------------------------
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
