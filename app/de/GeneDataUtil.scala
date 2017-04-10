package de

import java.io.InputStream

case class Gene(
  gene_id: String,
  gene_symbol: String,
  transcript_ids: Seq[String])

object Gene {

  def apply(line: String): Gene = {
    val spl = line.split("\t")
    Gene(spl(0), spl(1), spl(2).split(",").map { _.trim })
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

  /*  val jsonResponse: JsonNode = {
    val jsonInput = """{"query" : {"match_all" : {}},"fields": ["gene_info.symbol","gene_info.transcripts.transcript_id"]}"""
    val deploy = Seq("curl", "-X", "POST", s"http://ec2-54-167-120-156.compute-1.amazonaws.com:9200/pnoc_v2/genes/_search?size=60497&pretty", "-H", "Content-Type: application/json", "-d", jsonInput)
    val mapper: ObjectMapper = new ObjectMapper();
    mapper.registerModule(DefaultScalaModule)
    val response = (deploy.!!)
    val actualObj: JsonNode = mapper.readTree(response);
    actualObj.get("hits").get("hits");
  }*/

  val geneIds: Seq[String] = {
    genes.map { _.gene_id }
  }
  val geneSymbols: Seq[String] = {
    genes.map { _.gene_symbol }
  }
  val transcriptIds: Seq[String] = {
    genes.map { _.transcript_ids }.flatten.toSeq
  }

  def getGeneIds(): Seq[String] = geneIds
  def getGeneSymbols(): Seq[String] = geneSymbols
  def getTranscriptIds(): Seq[String] = transcriptIds

}