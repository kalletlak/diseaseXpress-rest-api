package de.v2.utils

import java.io.InputStream
import de.v2.model.Inputs.Gene
import de.v2.model.Inputs._
import de.v2.model.GeneInfoOutput

object GeneDataUtil {

  val geneLookup = {
    val stream: InputStream = getClass.getResourceAsStream("/gencode.v23.chr_patch_hapl_scaff.annotation_otherids.txt")
    val src = scala.io.Source.fromInputStream(stream)
    val x = src.getLines.map { Gene.apply }.toList
    src.close()
    x
  }

  private val genes =
    geneLookup
      .groupBy { _.gene_id }
      .mapValues { GeneInfoOutput.apply }

  private val transcripts =
    geneLookup
      .groupBy { _.transcript_id }
      .mapValues { GeneInfoOutput.apply }

  private val transcriptGeneMap =
    geneLookup
      .map { x => x.transcript_id -> x.gene_id }
      .toMap

  private val geneSymbolIdMap =
    geneLookup
      .map { x => x.gene_symbol -> x.gene_id }
      .toMap

  def getGeneInputRef(geneInputRef: GeneQueryRef): Option[GeneInfoOutput] = {
    geneInputRef match {
      case x: GeneIdQuery => {
        genes.get(x.ref_id)
      }
      case x: GeneSymbolQuery => {
        geneSymbolIdMap.get(x.ref_id) match {
          case Some(_gene_id) => genes.get(_gene_id)
          case _              => None
        }
      }
      case x: TranscriptIdQuery => {
        transcripts.get(x.ref_id)
      }
    }
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
