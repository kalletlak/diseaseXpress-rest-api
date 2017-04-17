package de

import java.io.InputStream

case class Gene(
  gene_id: String,
  gene_symbol: String,
  transcript_ids: Seq[String])

object Gene {

  def apply(line: String): Gene = {
    val spl = line.split("\t", -1).iterator
    Gene(
      gene_id = spl.next,
      gene_symbol = spl.next,
      transcript_ids = spl.next
        .split(",")
        .map { _.trim })
  }

}

object GeneDataUtil {

  val genes: Seq[Gene] = {
    val stream: InputStream = getClass.getResourceAsStream("/gencode.v23.annotation_id_mappings.txt")
    val src = scala.io.Source.fromInputStream(stream)
    val x = src.getLines.drop(1).map { Gene.apply }.toList
    src.close()
    x
  }

  val geneIds: Seq[String] = genes.map { _.gene_id }
  val geneSymbols: Seq[String] = genes.map { _.gene_symbol }
  val transcriptIds: Seq[String] = genes.map { _.transcript_ids }.flatten.toSeq

  def getGeneIds(): Seq[String] = geneIds
  def getGeneSymbols(): Seq[String] = geneSymbols
  def getTranscriptIds(): Seq[String] = transcriptIds

}