package de.utils

import scala.io.Source

import de.model.GeneInfo
import de.model.Inputs.{ GeneIdQuery, GeneQueryRef, GeneSymbolQuery, TranscriptIdQuery }
import io.swagger.annotations.ApiModel

case class Transcript(chr: String,
                      strand: String,
                      gene_symbol: String,
                      gene_id: String,
                      gene_start: Long,
                      gene_end: Long,
                      gene_biotype: String,
                      transcript_id: String,
                      transcript_start: Long,
                      transcript_end: Long,
                      transcript_biotype: String,
                      entrez_id: Option[Seq[String]],
                      refseq_mrna_id: Option[Seq[String]],
                      refseq_protein_id: Option[Seq[String]])

object Transcript {

  def apply(line: String): Transcript = {
    val itr = line.split("\t", 14).iterator
    Transcript(itr.next,
      itr.next,
      itr.next,
      itr.next,
      itr.next.toLong,
      itr.next.toLong,
      itr.next,
      itr.next,
      itr.next.toLong,
      itr.next.toLong,
      itr.next,
      toSeq(itr.next),
      toSeq(itr.next),
      toSeq(itr.next))
  }

  //convert empty value as empty list
  def toSeq(listAsString: String): Option[Seq[String]] = {
    listAsString match {
      case "" => None
      case nonEmptyListAsString => Some(nonEmptyListAsString.split(",", -1)
        .map(_.trim).toSeq)
    }
  }

}
object GeneDataUtil {

  val geneLookup = {

    //TODO: get url from configuration file
    val stream = Source.fromURL("https://s3.amazonaws.com/d3b.dam/disease-express/static-files/gencode.v23.annotation_otherids.txt")
    stream.getLines.drop(1).map {
      Transcript.apply
    }.toList
  }

  private val genes =
    geneLookup
      .groupBy {
        _.gene_id
      }
      .mapValues {
        GeneInfo.apply
      }

  private val transcripts =
    geneLookup
      .groupBy {
        _.transcript_id
      }
      .mapValues {
        GeneInfo.apply
      }

  private val geneSymbolIdMap =
    geneLookup
      .groupBy {
        _.gene_symbol
      }
      .mapValues { _.map { _.gene_id }.distinct }
      .toMap

  def getGeneInputRef(geneInputRef: GeneQueryRef): Seq[GeneInfo] = {
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
          .flatMap { _.map { genes.get } }
          .flatten
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

  def getGeneById(gene_id: String): Option[GeneInfo] = genes.get(gene_id)

  def getTranscript(transcript_id: String): Option[GeneInfo] = transcripts.get(transcript_id)

}
