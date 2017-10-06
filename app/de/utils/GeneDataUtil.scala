package de.utils

import scala.io.Source
import de.model.output.GeneInfo
import de.model.input.{ GeneIdQuery, GeneQueryRef, GeneSymbolQuery, TranscriptIdQuery }
import io.swagger.annotations.ApiModel

// ===========================================================================
object GeneDataUtil { // TODO: in-memory repo instead

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
  private val genes: Map[String /* gene ID */, GeneInfo] =
    transcriptsTmp
      .groupBy(_.gene_id)
      .mapValues(GeneInfo.apply)

  // ---------------------------------------------------------------------------      
  private val transcripts: Map[String /* transcript ID */, GeneInfo] =
    transcriptsTmp
      .groupBy(_.transcript_id)
      .mapValues(GeneInfo.apply)

  // ---------------------------------------------------------------------------      
  private val geneSymbolIdMap: Map[String /* gene symbol */, List[String /* gene ID */]] =
    transcriptsTmp
      .groupBy(_.gene_symbol)
      .mapValues(_.map(_.gene_id).distinct)
      .toMap

  // ===========================================================================    
  val getGeneIds:       Seq[String] = genes          .keySet.toSeq
  val getGeneSymbols:   Seq[String] = geneSymbolIdMap.keySet.toSeq
  val getTranscriptIds: Seq[String] = transcripts    .keySet.toSeq
      
  // ===========================================================================
  def getGeneInputRef(geneInputRef: GeneQueryRef): Seq[GeneInfo] = {
    geneInputRef
      match {
      
        case gene: GeneIdQuery =>
          gene
            .ref_id
            .flatMap(genes.get)
        
        case gene: GeneSymbolQuery =>
          gene
            .ref_id
            .flatMap { geneSymbolIdMap.get }
            .flatMap { _.map { genes.get } }
            .flatten        

        case gene: TranscriptIdQuery =>
          gene
            .ref_id
            .flatMap { transcripts.get }        
        
      }
    }

  // ---------------------------------------------------------------------------    
  def getGeneById  (gene_id: String):       Option[GeneInfo] = genes      .get(gene_id)
  def getTranscript(transcript_id: String): Option[GeneInfo] = transcripts.get(transcript_id)

}

// ===========================================================================

