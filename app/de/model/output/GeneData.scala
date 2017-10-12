package de.model.output

import de.utils.Enums.{Normalization, Projection}
import play.api.libs.json.Json

// ===========================================================================
case class GeneData(
    gene_id:     String,
    gene_symbol: String,
    data:        Seq[SampleData])

  // ===========================================================================
  object GeneData {
  
    implicit val WriteJson = Json.writes[GeneData]
  
    // ---------------------------------------------------------------------------
    def getHeader(
        rsem:                Boolean,
        sample_abundance:    Boolean,
        sample_rsem_isoform: Boolean,
        norms:               Seq[Normalization],
        projection:          Projection) =

      Seq(
          "gene_id",
          "gene_symbol",
          "data.sample_id") ++
        norms
        .map {
          _ match {
            
            case Normalization.rsem =>
              RsemGene
                .getHeader(projection)
                .map { "rsem." + _ }
              
            case Normalization.sample_abundance =>
              (Seq("transcript_id") ++
               Abundance
                  .getHeader(projection)
                  .map { "sample_abundance." + _ })
                .map { "transcripts." + _ }

            case Normalization.sample_rsem_isoform =>
              (Seq("transcript_id") ++
               RsemIsoform
                .getHeader(projection)
                .map { "sample_rsem_isoform." + _ })
              .map { "transcripts." + _ } } }
    
        .flatMap(_.map { "data." + _ })
        .distinct
  
    // ---------------------------------------------------------------------------
    def getValues(
        obj:        GeneData,
        norms:      Seq[Normalization],
        projection: Projection) =
      obj
        .data
        .flatMap { sampleData =>
          SampleData
            .getValues(sampleData, norms, projection)
            .map { Seq(obj.gene_id, obj.gene_symbol) ++ _ } }

  }

// ===========================================================================