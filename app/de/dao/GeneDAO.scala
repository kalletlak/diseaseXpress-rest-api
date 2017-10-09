package de.dao

import de.utils.Transcript
import scala.io.Source
import de.model.output.GeneInfo
import de.model.DomainTypes._

object GeneDAO {
  private val transcriptsTmp: List[Transcript] = {
    //TODO: get url from configuration file
    val stream =
      Source
        .fromURL("https://s3.amazonaws.com/d3b.dam/disease-express/static-files/gencode.v23.annotation_otherids.txt")

    stream // TODO: close
      .getLines
      .drop(1)
      .map(Transcript.apply)
      .toList
  }
  
    // ===========================================================================
  private val genes: Map[GeneId, GeneInfo] =
    transcriptsTmp
      .groupBy(_.gene_id)
      .mapValues(GeneInfo.apply)

  // ---------------------------------------------------------------------------      
  private val transcripts: Map[TranscriptId, GeneInfo] =
    transcriptsTmp
      .groupBy(_.transcript_id)
      .mapValues(GeneInfo.apply)

  // ---------------------------------------------------------------------------      
  private val geneSymbolIdMap: Map[GeneSymbol, List[String /* gene ID */]] =
    transcriptsTmp
      .groupBy(_.gene_symbol)
      .mapValues(_.map(_.gene_id).distinct)
      .toMap

  // ===========================================================================    
  def getGeneIds:       Seq[GeneId]       = genes          .keySet.toSeq
  def getGeneSymbols:   Seq[GeneSymbol]   = geneSymbolIdMap.keySet.toSeq
  def getTranscriptIds: Seq[TranscriptId] = transcripts    .keySet.toSeq
      
  // ===========================================================================
  
  def getGeneById  (gene_id: String):         Option[GeneInfo] = genes          .get(gene_id)
  def getTranscriptId(transcript_id: String): Option[GeneInfo] = transcripts    .get(transcript_id)
  def getGeneBySymbol(gene_id: String):       Seq[GeneInfo]    = geneSymbolIdMap.get(gene_id) match {
    case Some(gene_ids) => gene_ids.flatMap(genes.get)
    case _       => Seq()
  }
}