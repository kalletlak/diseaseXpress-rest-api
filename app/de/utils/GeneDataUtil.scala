package de.utils

import java.io.InputStream

import de.model.GeneInfoOutput
import de.model.Inputs.{ Gene, GeneIdQuery, GeneQueryRef, GeneSymbolQuery, TranscriptIdQuery }
import io.swagger.annotations.ApiModel

object GeneDataUtil {

  val geneLookup = {
    val stream: InputStream = getClass
      .getResourceAsStream("/gencode.v23.annotation_otherids.txt")
    val src = scala.io.Source.fromInputStream(stream)
    val x = src.getLines.drop(1).map {
      Gene.apply
    }.toList
    src.close()
    x
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

  private val transcriptGeneMap =
    geneLookup
      .map { x => x.transcript_id -> x.gene_id }
      .toMap

  private val geneSymbolIdMap =
    geneLookup
      .map { x => x.gene_symbol -> x.gene_id }
      .toMap

  def getGeneInputRef(geneInputRef: GeneQueryRef): Seq[GeneInfoOutput] = {
    val temp = geneInputRef match {
      case x: GeneIdQuery => {
        x.ref_id.map { x => genes.get(x) }

      }
      case x: GeneSymbolQuery => {
        x.ref_id.map { x =>
          geneSymbolIdMap.get(x) match {
            case Some(_gene_id) => genes.get(_gene_id)
            case _              => None
          }
        }

      }
      case x: TranscriptIdQuery => {
        x.ref_id.map { x =>  transcripts.get(x) }

      }
    }
    temp.flatten
  }

  def getGeneIds(): Seq[String] = genes.keySet.toSeq

  def getGeneSymbols(): Seq[String] = geneSymbolIdMap.keySet.toSeq

  def getTranscriptIds(): Seq[String] = transcriptGeneMap.keySet.toSeq

  def getGeneById(gene_id: String) = {
    genes.get(gene_id)
  }

  def getGeneBySymbol(gene_symbol: String) = {

    geneSymbolIdMap.get(gene_symbol) match {
      case Some(gene_id) => genes.get(gene_id)
      case _             => None
    }
  }

  def getTranscript(transcript_id: String) = {
    transcripts.get(transcript_id)
  }

}
