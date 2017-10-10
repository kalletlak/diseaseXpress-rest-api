package de.model.output

import de.utils.Enums.{Normalization, Projection}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json

// ===========================================================================
@ApiModel("SampleData")
case class SampleData(
    
    sample_id: String,

    @ApiModelProperty(
      name = "SampleRsemGeneData",
      dataType = "de.model.output.RsemGene",
      required = false)
    rsem: Option[RsemGene],

    @ApiModelProperty(
      name = "TranscriptData",
      dataType = "de.model.output.TranscriptData",
      required = false)
    transcripts: Option[Seq[TranscriptData]])

  // ===========================================================================
  object SampleData {
  
    implicit val WriteJson = Json.writes[SampleData]
  
    // ---------------------------------------------------------------------------
    def getValues(
        obj:        SampleData,
        norms:      Seq[Normalization],
        projection: Projection) = { // FIXME: anys
      
      val sample_rsem_values =
        obj
          .rsem
           match {
        
            case Some(data) =>
              RsemGene.getValues(data, projection)
              
            case _ => // FIXME
              if (norms.contains(Normalization.rsem)) // TODO: anti-pattern
                RsemGene.getValues(RsemGene(), projection)
              else
                Seq()
            
          }
  
      val sample_rsem_values_with_sample_id = // FIXME: Anys
        Seq(obj.sample_id) ++
        sample_rsem_values

      obj
        .transcripts
         match {
        
          case Some(transcripts) =>
            transcripts
              .map { transcript =>
                sample_rsem_values_with_sample_id ++
                TranscriptData.getValues(transcript, norms, projection) }

          case _ => // FIXME: risky
            Seq(sample_rsem_values_with_sample_id)
          
      }
  
    }
    
  }

// ===========================================================================