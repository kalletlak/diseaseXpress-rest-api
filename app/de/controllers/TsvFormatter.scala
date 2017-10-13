package de.controllers

import de.model.output.{GeneInfo, GeneData, TranscriptWithGeneInfo}
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import de.utils.Enums.Projection
import de.utils.Enums.Normalization
import play.api.libs.json.JsObject

// ===========================================================================
object TsvFormatter { // TODO: refactor all these, they all do the same thing
  
  private val TsvMissing = ""
  
  // ---------------------------------------------------------------------------
  def apply(response: Seq[Map[String, JsValue]]): String = {      
    val header: Seq[String] =
      response
        .flatMap(_.keySet)
        .distinct
    
    val data: Seq[Seq[String]] =
      response
        .map { tags =>
          header
            .map(tags.get)
            .map(_.getOrElse(JsString(TsvMissing)))
            .map(_.toString) }
    
    (header +: data)
      .map(_.mkString("\t"))
      .mkString("\n")         
  }
  
  // ===========================================================================
  /** converts gene data object to tsv format */
  def geneInfo(genes: Seq[GeneInfo]) = {
    val header = Seq(GeneInfo.getHeader.mkString("\t"))
    val data = genes
      .flatMap(GeneInfo.getValues)
      .map(_.mkString("\t"))

    (header ++ data).mkString("\n")
  }

  // =========================================================================== 
  /** converts transcript data object to tsv format */
  def transcriptInfo(transcripts: Seq[TranscriptWithGeneInfo]) = {
  
    val header = Seq(TranscriptWithGeneInfo
      .getHeader
      .mkString("\t"))

    val data = transcripts
      .map { TranscriptWithGeneInfo.getValues }
      .map { _.mkString("\t") }

    (header ++ data).mkString("\n")
  }
  
  // ===========================================================================
  /** converts gene data object to tsv format */ 
  def rnaSeq(
        result:         Seq[GeneData],
        normalizations: Seq[Normalization],
        projection:     Projection)
      : String = {

    val rsem: Boolean =
      normalizations.contains(Normalization.rsem)
      
    val sample_abundance: Boolean =
      normalizations.contains(Normalization.sample_abundance)
      
    val sample_rsem_isoform: Boolean =
      normalizations.contains(Normalization.sample_rsem_isoform)

    val header =
      GeneData
        .getHeader(rsem, sample_abundance, sample_rsem_isoform, normalizations, projection)
        .mkString("\t")

    val data =
      result
        .map { geneData =>
          GeneData
            .getValues(geneData, normalizations, projection)
            .map(_.mkString("\t"))
            .mkString("\n") }

    (header +: data).mkString("\n")
  }  
}

// ===========================================================================
