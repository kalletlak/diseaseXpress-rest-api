package de.model.output

import de.utils.Enums.{Normalization, Projection}
import play.api.libs.json.Json
import de.utils.Transcript
import utils.OptionUtils

// ===========================================================================
case class TranscriptInfo(
    transcript_id:      String,
    start:              Long,
    end:                Long,
    biotype:            String,
    entrez_ids:         Option[Seq[String]],
    refseq_mrna_ids:    Option[Seq[String]],
    refseq_protein_ids: Option[Seq[String]])

  // ===========================================================================
  object TranscriptInfo {  
    implicit val WriteJson = Json.writes[TranscriptInfo]
    
    // ---------------------------------------------------------------------------
    def apply(obj: Transcript): TranscriptInfo = {
      TranscriptInfo(
        obj.transcript_id,
        obj.transcript_start,
        obj.transcript_end,
        obj.transcript_biotype,
        obj.entrez_id,
        obj.refseq_mrna_id,
        obj.refseq_protein_id)
    }
  
    // ---------------------------------------------------------------------------
    def getHeader =
      Seq(
        "transcript_id",
        "start",
        "end",
        "biotype",
        "entrez_ids",
        "refseq_mrna_ids",
        "refseq_protein_ids")
  
    // ---------------------------------------------------------------------------
    def getValues(obj: TranscriptInfo) =
      Seq(
        obj.transcript_id,
        obj.start,
        obj.end,
        obj.biotype,
        OptionUtils.arrayToString(obj.entrez_ids),
        OptionUtils.arrayToString(obj.refseq_mrna_ids),
        OptionUtils.arrayToString(obj.refseq_protein_ids))
        
  }

// ===========================================================================