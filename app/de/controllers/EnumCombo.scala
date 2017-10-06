package de.controllers

import de.utils.Enums.Projection
import de.utils.Enums.Normalization
import de.model.Inputs.InputDataModel
import de.model.Inputs.RsemGeneProjectons
import de.model.Inputs.AbundanceProjectons
import de.model.Inputs.RsemIsoformProjectons

// ===========================================================================
object EnumCombo {
  
  def apply(
          projection:     Projection,
          normalizations: Seq[Normalization])
      : Seq[(Normalization, InputDataModel)] =
    projection match {
      
      case Projection.detailed =>
        normalizations
          .map { normalization =>
            (normalization, detailed(normalization)) }

      case Projection.summary =>
        normalizations
          .map { normalization =>
            (normalization, summary(normalization)) } }
 
  // ---------------------------------------------------------------------------
  private def detailed(normalization: Normalization) =
    normalization match {
    
      case Normalization.rsem =>
        RsemGeneProjectons(
          length           = true,
          effective_length = true,
          expected_count   = true,
          tpm              = true,
          fpkm             = true)
        
      case Normalization.sample_abundance =>
        AbundanceProjectons(
          length           = true,
          effective_length = true,
          expected_count   = true,
          tpm              = true)
        
      case Normalization.sample_rsem_isoform =>
        RsemIsoformProjectons(
          length             = true,
          effective_length   = true,
          expected_count     = true,
          tpm                = true,
          fpkm               = true,
          isoform_percentage = true)
  }

  // ---------------------------------------------------------------------------
  private def summary(normalization: Normalization) =
    normalization match {                
      case Normalization.rsem                => RsemGeneProjectons()        
      case Normalization.sample_abundance    => AbundanceProjectons()
      case Normalization.sample_rsem_isoform => RsemIsoformProjectons()
    }
  
}

// ===========================================================================
