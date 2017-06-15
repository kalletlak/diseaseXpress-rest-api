package de.v2.model

import scala.Right
import io.swagger.annotations.ApiModel
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{ JsObject, JsResult, JsSuccess, JsValue, Json, Writes }
import play.api.libs.json.JsValue.jsValueToJsLookup

import de.v2.model.Inputs._
import de.v2.utils.JsObjectWithOption
import de.v2.utils.PlayJsonUtils.JsObjectImplicits

@ApiModel("SampleRsemIsoformData")
case class SampleRsemIsoformOutput(
    transcript_id: String,
    sample_id: String,
    @ApiModelProperty(dataType = "double", required = false) length: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) effective_length: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) expected_count: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) tpm: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) fpkm: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) isoform_percentage: Option[Double]) {
  @ApiModelProperty(hidden = true)
  def getHeader(projection: String) = projection match {
    case "detailed" => Seq("length", "effective_length", "expected_count", "tpm", "fpkm", "isoform_percentage")
    case "summary"  => Seq("tpm")
  }
  @ApiModelProperty(hidden = true)
  def getValues(projection: String) = projection match {
    case "detailed" => Seq(this.length.get, this.effective_length.get, this.expected_count.get, this.tpm.get, this.fpkm.get, this.isoform_percentage.get)
    case "summary"  => Seq(this.tpm.get)
  }
}

object SampleRsemIsoformOutput {

  def readJson(transcript_id: String, obj: JsObject, projectionFileds: SampleRsemIsoformProjectons = new SampleRsemIsoformProjectons): JsResult[SampleRsemIsoformOutput] =
    JsSuccess(SampleRsemIsoformOutput(
      transcript_id = transcript_id,
      sample_id = (obj \ "sample_id").as[String],
      length = obj.parseDoubleOption("length", projectionFileds.length),
      effective_length = obj.parseDoubleOption("effective_length", projectionFileds.effective_length),
      expected_count = obj.parseDoubleOption("expected_count", projectionFileds.expected_count),
      tpm = obj.parseDoubleOption("tpm", projectionFileds.tpm),
      fpkm = obj.parseDoubleOption("fpkm", projectionFileds.fpkm),
      isoform_percentage = obj.parseDoubleOption("isoform_percentage", projectionFileds.isoform_percentage)))

  implicit val writeJson = new Writes[SampleRsemIsoformOutput] {
    def writes(obj: SampleRsemIsoformOutput): JsValue = {
      JsObjectWithOption("length" -> Right(obj.length.map(x => Json.toJson(x))),
        "effective_length" -> Right(obj.effective_length.map(x => Json.toJson(x))),
        "expected_count" -> Right(obj.expected_count.map(x => Json.toJson(x))),
        "tpm" -> Right(obj.tpm.map(x => Json.toJson(x))),
        "fpkm" -> Right(obj.fpkm.map(x => Json.toJson(x))),
        "isoform_percentage" -> Right(obj.isoform_percentage.map(x => Json.toJson(x))))
    }
  }
}

trait SampleData
@ApiModel("SampleAbundanceData")
case class SampleAbundanceOutput(
    transcript_id: String,
    sample_id: String,
    @ApiModelProperty(dataType = "double", required = false) length: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) effective_length: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) expected_count: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) tpm: Option[Double]) extends SampleData {
  @ApiModelProperty(hidden = true)
  def getHeader(projection: String) = projection match {
    case "detailed" => Seq("length", "effective_length", "expected_count", "tpm")
    case "summary"  => Seq("tpm")
  }
  @ApiModelProperty(hidden = true)
  def getValues(projection: String) = projection match {
    case "detailed" => Seq(this.length.get, this.effective_length.get, this.expected_count.get, this.tpm.get)
    case "summary"  => Seq(this.tpm.get)
  }
}
object SampleAbundanceOutput {

  def readJson(transcript_id: String, obj: JsObject, projectionFileds: SampleAbundanceProjectons = new SampleAbundanceProjectons): JsResult[SampleAbundanceOutput] = {
    JsSuccess(SampleAbundanceOutput(
      transcript_id = transcript_id,
      sample_id = (obj \ "sample_id").as[String],
      length = obj.parseDoubleOption("length", projectionFileds.length),
      effective_length = obj.parseDoubleOption("effective_length", projectionFileds.effective_length),
      expected_count = obj.parseDoubleOption("expected_count", projectionFileds.expected_count),
      tpm = obj.parseDoubleOption("tpm", projectionFileds.tpm)))
  }
  implicit val writeJson = new Writes[SampleAbundanceOutput] {
    def writes(obj: SampleAbundanceOutput): JsValue = {
      JsObjectWithOption("length" -> Right(obj.length.map(x => Json.toJson(x))),
        "effective_length" -> Right(obj.effective_length.map(x => Json.toJson(x))),
        "expected_count" -> Right(obj.expected_count.map(x => Json.toJson(x))),
        "tpm" -> Right(obj.tpm.map(x => Json.toJson(x))))
    }
  }
}
@ApiModel("SampleRsemGeneData")
case class SampleRsemGeneOutput(
    gene_id: String,
    sample_id: String,
    @ApiModelProperty(dataType = "double", required = false) length: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) effective_length: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) expected_count: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) tpm: Option[Double],
    @ApiModelProperty(dataType = "double", required = false) fpkm: Option[Double]) extends SampleData {
  @ApiModelProperty(hidden = true)
  def getHeader(projection: String) = projection match {
    case "detailed" => Seq("length", "effective_length", "expected_count", "tpm", "fpkm")
    case "summary"  => Seq("fpkm")
  }
  @ApiModelProperty(hidden = true)
  def getValues(projection: String) = projection match {
    case "detailed" => Seq(this.length.get, this.effective_length.get, this.expected_count.get, this.tpm.get, this.fpkm.get)
    case "summary"  => Seq(this.fpkm.get)
  }
}
object SampleRsemGeneOutput {
  def readJson(gene_id: String, obj: JsObject, projectionFileds: SampleRsemGeneProjectons = new SampleRsemGeneProjectons()): JsResult[SampleRsemGeneOutput] =
    JsSuccess(SampleRsemGeneOutput(
      gene_id = gene_id,
      sample_id = (obj \ "sample_id").as[String],
      length = obj.parseDoubleOption("length", projectionFileds.length),
      effective_length = obj.parseDoubleOption("effective_length", projectionFileds.effective_length),
      expected_count = obj.parseDoubleOption("expected_count", projectionFileds.expected_count),
      tpm = obj.parseDoubleOption("tpm", projectionFileds.tpm),
      fpkm = obj.parseDoubleOption("fpkm", projectionFileds.fpkm)))
  implicit val writeJson = new Writes[SampleRsemGeneOutput] {
    def writes(obj: SampleRsemGeneOutput): JsValue = {
      JsObjectWithOption("length" -> Right(obj.length.map(x => Json.toJson(x))),
        "effective_length" -> Right(obj.effective_length.map(x => Json.toJson(x))),
        "expected_count" -> Right(obj.expected_count.map(x => Json.toJson(x))),
        "tpm" -> Right(obj.tpm.map(x => Json.toJson(x))),
        "fpkm" -> Right(obj.fpkm.map(x => Json.toJson(x))))
    }
  }
}
@ApiModel("TranscriptData")
case class TranscriptLevelOutput(
    transcript_id: String,
    @ApiModelProperty(name = "SampleAbundanceData", dataType = "de.v2.model.SampleAbundanceOutput", required = false) sample_abundance: Option[SampleAbundanceOutput],
    @ApiModelProperty(name = "SampleRsemIsoformData", dataType = "de.v2.model.SampleRsemIsoformOutput", required = false) sample_rsem_isoform: Option[SampleRsemIsoformOutput]) extends SampleData {
  @ApiModelProperty(hidden = true)
  def getHeader(projection: String) =
    Seq("transcript_id") ++ (if (sample_abundance.isDefined) sample_abundance.get.getHeader(projection).map { x => s"""sample_abundance.$x""" } else Seq()) ++ (if (sample_rsem_isoform.isDefined) sample_rsem_isoform.get.getHeader(projection).map { x => s"""sample_rsem_isoform.$x""" } else Seq())

  @ApiModelProperty(hidden = true)
  def getValues(projection: String) = {
    Seq(transcript_id) ++ (if (sample_abundance.isDefined) sample_abundance.get.getValues(projection) else Seq()) ++ (if (sample_rsem_isoform.isDefined) sample_rsem_isoform.get.getValues(projection) else Seq())

  }
}
object TranscriptLevelOutput {
  implicit val WriteJson = Json.writes[TranscriptLevelOutput]
}

@ApiModel("SampleData")
case class SampleDataOutput(
    sample_id: String,
    @ApiModelProperty(name = "SampleRsemGeneData", dataType = "de.v2.model.SampleRsemGeneOutput", required = false) rsem: Option[SampleRsemGeneOutput],
    @ApiModelProperty(name = "TranscriptData", dataType = "de.v2.model.TranscriptLevelOutput", required = false) transcripts: Option[Seq[TranscriptLevelOutput]]) {
  @ApiModelProperty(hidden = true)
  def getHeader(projection: String) =
    Seq("sample_id") ++ (if (rsem.isDefined) rsem.get.getHeader(projection).map { x => s"""rsem.$x""" } else Seq()) ++ (if (transcripts.isDefined) transcripts.get.head.getHeader(projection).map { x => s"""transcripts.$x""" } else Seq())

  @ApiModelProperty(hidden = true)
  def getValues(projection: String) = {
    val temp = Seq(sample_id) ++ (if (rsem.isDefined) rsem.get.getValues(projection) else Seq())
    if (transcripts.isDefined) {
      transcripts.get.map { transcript => temp ++ transcript.getValues(projection) }
    } else Seq(temp)

  }
}
object SampleDataOutput {
  implicit val WriteJson = Json.writes[SampleDataOutput]
}
@ApiModel("GeneData")
case class GeneLevelOutput(
    gene_id: String,
    gene_symbol: String,
    @ApiModelProperty(name = "SampleData") data: Seq[SampleDataOutput]) {
  @ApiModelProperty(hidden = true)
  def getHeader(projection: String) =
    Seq("gene_id", "gene_symbol") ++ (if (data.size > 0) data.head.getHeader(projection) else Seq())

  @ApiModelProperty(hidden = true)
  def getValues(projection: String) = {
    val temp = Seq(gene_id, gene_symbol)
    data.flatMap { sampleDataOutput =>
      sampleDataOutput.getValues(projection).map { x => temp ++ x }
    }
  }
}
object GeneLevelOutput {
  implicit val WriteJson = Json.writes[GeneLevelOutput]
}

@ApiModel("TranscriptWithGeneInfo")
case class TranscriptWithGeneInfoOutput(
  transcript_id: String,
  start: Long,
  end: Long,
  biotype: String,
  entrez_ids: Seq[String],
  refseq_mrna_ids: Seq[String],
  refseq_protein_ids: Seq[String],
  gene_id: String,
  gene_symbol: String)

object TranscriptWithGeneInfoOutput {
  def apply(obj: TranscriptInfoOutput, gene_id: String, gene_symbol: String): TranscriptWithGeneInfoOutput = {
    TranscriptWithGeneInfoOutput(obj.transcript_id, obj.start, obj.end, obj.biotype, obj.entrez_ids, obj.refseq_mrna_ids, obj.refseq_protein_ids, gene_id, gene_symbol)
  }
  implicit val WriteJson = Json.writes[TranscriptWithGeneInfoOutput]

  def getHeader = Seq("transcript_id", "start", "end", "biotype", "entrez_ids", "refseq_mrna_ids", "refseq_protein_ids", "gene_id", "gene_symbol")

  def getValues(obj: TranscriptWithGeneInfoOutput) =
    Seq(obj.transcript_id, obj.start, obj.end, obj.biotype, obj.entrez_ids.mkString(","), obj.refseq_mrna_ids.mkString(","), obj.refseq_protein_ids.mkString(","), obj.gene_id, obj.gene_symbol)

}

@ApiModel("Transcript")
case class TranscriptInfoOutput(
  transcript_id: String,
  start: Long,
  end: Long,
  biotype: String,
  entrez_ids: Seq[String],
  refseq_mrna_ids: Seq[String],
  refseq_protein_ids: Seq[String])
object TranscriptInfoOutput {
  def apply(geneTranscriptInfo: Gene): TranscriptInfoOutput = {
    TranscriptInfoOutput(
      geneTranscriptInfo.transcript_id,
      geneTranscriptInfo.transcript_start,
      geneTranscriptInfo.transcript_end,
      geneTranscriptInfo.transcript_biotype,
      geneTranscriptInfo.entrez_id,
      geneTranscriptInfo.refseq_mrna_id,
      geneTranscriptInfo.refseq_protein_id)
  }
  implicit val WriteJson = Json.writes[TranscriptInfoOutput]

  def getHeader = Seq("transcript_id", "start", "end", "biotype", "entrez_ids", "refseq_mrna_ids", "refseq_protein_ids")

  def getValues(obj: TranscriptInfoOutput) = Seq(obj.transcript_id, obj.start, obj.end, obj.biotype, obj.entrez_ids.mkString(","), obj.refseq_mrna_ids.mkString(","), obj.refseq_protein_ids.mkString(","))
}
@ApiModel(value = "Gene")
case class GeneInfoOutput(
  gene_id: String,
  gene_symbol: String,
  start: Long,
  end: Long,
  biotype: String,
  chr: String,
  strand: String,
  transcripts: Seq[TranscriptInfoOutput])

object GeneInfoOutput {
  def apply(geneTranscriptInfo: Seq[Gene]): GeneInfoOutput = {
    GeneInfoOutput(geneTranscriptInfo(0).gene_id,
      geneTranscriptInfo(0).gene_symbol,
      geneTranscriptInfo(0).gene_start,
      geneTranscriptInfo(0).gene_end,
      geneTranscriptInfo(0).gene_biotype,
      geneTranscriptInfo(0).chr,
      geneTranscriptInfo(0).strand,
      geneTranscriptInfo.map { TranscriptInfoOutput.apply })
  }
  implicit val WriteJson = Json.writes[GeneInfoOutput]

  def getHeader = Seq("gene_id", "gene_symbol", "start", "end", "biotype", "chr", "strand") ++ TranscriptInfoOutput.getHeader

  def getValues(obj: GeneInfoOutput) = {
    val _genedata = Seq(obj.gene_id, obj.gene_symbol, obj.start, obj.end, obj.biotype, obj.chr, obj.strand)
    obj.transcripts.map { transcript => _genedata ++ TranscriptInfoOutput.getValues(transcript) }
  }

}
