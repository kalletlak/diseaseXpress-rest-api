package de.utils

import scala.io.Source

import de.model.GeneInfoOutput
import de.model.Inputs.{ Gene, GeneIdQuery, GeneQueryRef, GeneSymbolQuery, TranscriptIdQuery }
import io.swagger.annotations.ApiModel

object GeneDataUtil {

  val geneLookup = {

    //TODO: get url from configuration file
    val stream = Source.fromURL("https://s3.amazonaws.com/d3b.dam/disease-express/static-files/gencode.v23.annotation_otherids.txt")
    stream.getLines.drop(1).map {
      Gene.apply
    }.toList
  }

  private val genes =
    geneLookup
      .groupBy {
        _.gene_id
      }
      .mapValues {
        GeneInfoOutput.apply
      }

  private val transcripts =
    geneLookup
      .groupBy {
        _.transcript_id
      }
      .mapValues {
        GeneInfoOutput.apply
      }

  private val geneSymbolIdMap =
    geneLookup
      .map { gene => gene.gene_symbol -> gene.gene_id }
      .toMap

  def getGeneInputRef(geneInputRef: GeneQueryRef): Seq[GeneInfoOutput] = {
    geneInputRef match {
      case gene: GeneIdQuery => {
        gene
          .ref_id
          .flatMap { genes.get }

      }
      case gene: GeneSymbolQuery => {
        gene
          .ref_id
          .flatMap { geneSymbolIdMap.get }
          .flatMap { genes.get }

      }
      case gene: TranscriptIdQuery => {
        gene
          .ref_id
          .flatMap { transcripts.get }
      }
    }
  }

  val getGeneIds: Seq[String] = genes.keySet.toSeq

  val getGeneSymbols: Seq[String] = geneSymbolIdMap.keySet.toSeq

  val getTranscriptIds: Seq[String] = transcripts.keySet.toSeq

  def getGeneById(gene_id: String): Option[GeneInfoOutput] = genes.get(gene_id)

  def getTranscript(transcript_id: String): Option[GeneInfoOutput] = transcripts.get(transcript_id)

}
