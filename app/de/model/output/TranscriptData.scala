package de.model.output

import de.utils.Enums.{Normalization, Projection}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json

// ===========================================================================
@ApiModel("TranscriptData")
case class TranscriptData(
    
      transcript_id: String,
      
      @ApiModelProperty(
        name = "SampleAbundanceData",
        dataType = "de.model.output.Abundance",
        required = false)
      sample_abundance: Option[Abundance],
                              
      @ApiModelProperty(
        name = "RsemIsoformData",
        dataType = "de.model.output.RsemIsoform",
        required = false)
      sample_rsem_isoform: Option[RsemIsoform])

  // ===========================================================================
  object TranscriptData {
  
    implicit val WriteJson = Json.writes[TranscriptData]
  
    // ---------------------------------------------------------------------------
    def getValues(
        obj:        TranscriptData,
        norms:      Seq[Normalization],
        projection: Projection) = {

      val sample_abundance_values =
        obj
          .sample_abundance
           match {
        
            case Some(data) =>
              Abundance.getValues(data, projection)
              
            case _ =>
              if (norms.contains(Normalization.sample_abundance))
                Abundance.getValues(Abundance(), projection)
              else
                Seq()            

          }
  
      val sample_rsem_isoform_values =
        obj
          .sample_rsem_isoform
           match {
        
            case Some(data) =>
              RsemIsoform.getValues(data, projection)

            case _ =>
              if (norms.contains(Normalization.rsem))
                RsemIsoform
                  .getValues(RsemIsoform(), projection)
              else
                Seq()
                
          }
      
      Seq(obj.transcript_id) ++
      sample_abundance_values ++
      sample_rsem_isoform_values  
    }
    
  }

// ===========================================================================
