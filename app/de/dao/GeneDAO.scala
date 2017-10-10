package de.dao

import scala.io.Source

import de.model.DomainTypes.{ GeneId, GeneSymbol, TranscriptId }
import de.model.output.GeneInfo
import de.utils.Transcript
import io.swagger.annotations.ApiModel

object GeneDAO {
  private val transcriptsTmp: List[Transcript] = {
    //TODO: get url from configuration file
    val stream =
      Source
        .fromURL("https://s3.amazonaws.com/d3b.dam/disease-express/static-files/gencode.v23.annotation_otherids.txt")

    val results = stream
      .getLines
      .drop(1)
      .map(Transcript.apply)
      .toList

    stream.close()
    results
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
  private val geneSymbolIdMap: Map[GeneSymbol, List[GeneId]] =
    transcriptsTmp
      .groupBy(_.gene_symbol)
      .mapValues(_.map(_.gene_id).distinct)
      .toMap

  // ===========================================================================    
  def getGeneIds:       Seq[GeneId]       = genes          .keySet.toSeq
  def getGeneSymbols:   Seq[GeneSymbol]   = geneSymbolIdMap.keySet.toSeq
  def getTranscriptIds: Seq[TranscriptId] = transcripts    .keySet.toSeq
      
  def getGeneById     (gene_id: GeneId):             Option[GeneInfo] = genes.get(gene_id)
  
  def getTranscriptId (transcript_id: TranscriptId): Option[GeneInfo] = transcripts.get(transcript_id)
  
  def getGeneBySymbol (gene_id: GeneId):             Seq[GeneInfo]    = 
    geneSymbolIdMap
      .getOrElse(gene_id, Seq())
      .flatMap { genes.get }
  
  
  // ===========================================================================

}