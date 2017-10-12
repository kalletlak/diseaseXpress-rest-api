package de.model.output

import de.utils.Transcript
import de.utils.Enums.{Normalization, Projection}
import play.api.libs.json.Json

// ===========================================================================
case class GeneInfo(
    gene_id:     String,
    gene_symbol: String,
    start:       Long,
    end:         Long,
    biotype:     String,
    chr:         String,
    strand:      String,
    transcripts: Seq[TranscriptInfo])

  // ===========================================================================
  object GeneInfo {
  
    implicit val WriteJson = Json.writes[GeneInfo]
  
    // ---------------------------------------------------------------------------
    def apply(geneTranscriptInfo: Seq[Transcript]): GeneInfo = {
      val head: Transcript = geneTranscriptInfo.head
      
      GeneInfo(
        head.gene_id,
        head.gene_symbol,
        head.gene_start,
        head.gene_end,
        head.gene_biotype,
        head.chr,
        head.strand,
        
        geneTranscriptInfo.map(TranscriptInfo.apply))
    }
    
    // ---------------------------------------------------------------------------
    def getHeader =
      Seq(
          "gene_id",
          "gene_symbol",
          "start",
          "end",
          "biotype",
          "chr",
          "strand") ++
        TranscriptInfo
          .getHeader
          .map { "transcripts." + _ }
  
    // ---------------------------------------------------------------------------
    def getValues(obj: GeneInfo) = {
      val _genedata =
        Seq(
          obj.gene_id,
          obj.gene_symbol,
          obj.start,
          obj.end,
          obj.biotype,
          obj.chr,
          obj.strand)

      obj.transcripts
        .map { transcript =>
          _genedata ++
          TranscriptInfo.getValues(transcript) }
    }
  
  }

// ===========================================================================
