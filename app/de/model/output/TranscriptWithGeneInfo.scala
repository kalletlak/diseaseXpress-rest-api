package de.model.output

import de.utils.Enums.{Normalization, Projection}
import play.api.libs.json.Json
import utils.OptionUtils

// ===========================================================================
case class TranscriptWithGeneInfo(
    transcript_id:      String,
    start:              Long,
    end:                Long,
    biotype:            String,
    entrez_ids:         Option[Seq[String]],
    refseq_mrna_ids:    Option[Seq[String]],
    refseq_protein_ids: Option[Seq[String]],
    gene_id:            String,
    gene_symbol:        String)

  // ===========================================================================
  object TranscriptWithGeneInfo {    
  
    implicit val WriteJson = Json.writes[TranscriptWithGeneInfo]

    // ---------------------------------------------------------------------------
    def apply
        (gene_id: String,
         gene_symbol: String)
        (gene: TranscriptInfo)
      : TranscriptWithGeneInfo =
        TranscriptWithGeneInfo(
          gene.transcript_id,
          gene.start,
          gene.end,
          gene.biotype,
          gene.entrez_ids,
          gene.refseq_mrna_ids,
          gene.refseq_protein_ids,
          gene_id,
          gene_symbol)  
  
    // ---------------------------------------------------------------------------
    def getHeader =
      Seq(
        "transcript_id",
        "start",
        "end",
        "biotype",
        "entrez_ids",
        "refseq_mrna_ids",
        "refseq_protein_ids",
        "gene_id",
        "gene_symbol")
  
    // ---------------------------------------------------------------------------
    def getValues(obj: TranscriptWithGeneInfo) =
      Seq(obj.transcript_id,
        obj.start,
        obj.end,
        obj.biotype,
        OptionUtils.arrayToString(obj.entrez_ids),
        OptionUtils.arrayToString(obj.refseq_mrna_ids),
        OptionUtils.arrayToString(obj.refseq_protein_ids),
        obj.gene_id,
        obj.gene_symbol)
        
  }

// ===========================================================================