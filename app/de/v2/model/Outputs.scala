package de.v2.model

import scala.Right
import io.swagger.annotations.ApiModel
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{ JsObject, JsResult, JsSuccess, JsValue, Json, Writes }
import play.api.libs.json.JsValue.jsValueToJsLookup

import de.v2.model.Inputs._
import de.v2.utils.JsObjectWithOption
import de.v2.utils.PlayJsonUtils.JsObjectImplicits
import de.v2.utils.Enums.Projection
import de.v2.utils.Enums.Normalization

//initialized parameters with default values. This would be used in getting tsv format data
//when querying for a particular normalization and doesn't have any data in the database
@ApiModel("SampleRsemIsoformData")
case class SampleRsemIsoformOutput(
  transcript_id: String = "",
  sample_id: String = "",
  @ApiModelProperty(
    dataType = "double",
    required = false) length: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) effective_length: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) expected_count: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) tpm: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) fpkm: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) isoform_percentage: Option[Double] = None)

object SampleRsemIsoformOutput {

  def readJson(transcript_id: String,
               obj: JsObject,
               projectionFileds: ObjectFilters = new SampleRsemIsoformProjectons): JsResult[SampleRsemIsoformOutput] = {
    val projectionFiledsObj = projectionFileds.asInstanceOf[SampleRsemIsoformProjectons]
    JsSuccess(SampleRsemIsoformOutput(
      transcript_id = transcript_id,
      sample_id = (obj \ "sample_id").as[String],
      length = obj.parseDoubleOption("length", projectionFiledsObj.length),
      effective_length = obj.parseDoubleOption("effective_length", projectionFiledsObj.effective_length),
      expected_count = obj.parseDoubleOption("expected_count", projectionFiledsObj.expected_count),
      tpm = obj.parseDoubleOption("tpm", projectionFiledsObj.tpm),
      fpkm = obj.parseDoubleOption("fpkm", projectionFiledsObj.fpkm),
      isoform_percentage = obj.parseDoubleOption("isoform_percentage", projectionFiledsObj.isoform_percentage)))
  }

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

  def getHeader(projection: Projection) = projection match {
    case Projection.detailed => Seq("length",
      "effective_length",
      "expected_count",
      "tpm",
      "fpkm",
      "isoform_percentage")
    case Projection.summary => Seq("tpm")
  }

  def getValues(obj: SampleRsemIsoformOutput, projection: Projection) = projection match {
    case Projection.detailed => Seq(obj.length.getOrElse(""),
      obj.effective_length.getOrElse(""),
      obj.expected_count.getOrElse(""),
      obj.tpm.getOrElse(""),
      obj.fpkm.getOrElse(""),
      obj.isoform_percentage.getOrElse(""))
    case Projection.summary => Seq(obj.tpm.getOrElse(""))
  }
}

//initialized parameters with default values. used would be used when getting tsv format data
@ApiModel("SampleAbundanceData")
case class SampleAbundanceOutput(
  transcript_id: String = "",
  sample_id: String = "",
  @ApiModelProperty(
    dataType = "double",
    required = false) length: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) effective_length: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) expected_count: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) tpm: Option[Double] = None)

object SampleAbundanceOutput {

  def readJson(transcript_id: String,
               obj: JsObject,
               projectionFileds: ObjectFilters = new SampleAbundanceProjectons): JsResult[SampleAbundanceOutput] = {
    val projectionFiledsObj = projectionFileds.asInstanceOf[SampleAbundanceProjectons]

    JsSuccess(SampleAbundanceOutput(
      transcript_id = transcript_id,
      sample_id = (obj \ "sample_id").as[String],
      length = obj.parseDoubleOption("length", projectionFiledsObj.length),
      effective_length = obj.parseDoubleOption("effective_length", projectionFiledsObj.effective_length),
      expected_count = obj.parseDoubleOption("expected_count", projectionFiledsObj.expected_count),
      tpm = obj.parseDoubleOption("tpm", projectionFiledsObj.tpm)))
  }

  implicit val writeJson = new Writes[SampleAbundanceOutput] {
    def writes(obj: SampleAbundanceOutput): JsValue = {
      JsObjectWithOption("length" -> Right(obj.length.map(x => Json.toJson(x))),
        "effective_length" -> Right(obj.effective_length.map(x => Json.toJson(x))),
        "expected_count" -> Right(obj.expected_count.map(x => Json.toJson(x))),
        "tpm" -> Right(obj.tpm.map(x => Json.toJson(x))))
    }
  }

  def getHeader(projection: Projection) = projection match {
    case Projection.detailed => Seq("length",
      "effective_length",
      "expected_count",
      "tpm")
    case Projection.summary => Seq("tpm")
  }

  def getValues(obj: SampleAbundanceOutput,
                projection: Projection) = projection match {
    case Projection.detailed => Seq(obj.length.getOrElse(""),
      obj.effective_length.getOrElse(""),
      obj.expected_count.getOrElse(""),
      obj.tpm.getOrElse(""))
    case Projection.summary => Seq(obj.tpm.getOrElse(""))
  }

}

//initialized parameters with default values. used would be used when getting tsv format data
@ApiModel("SampleRsemGeneData")
case class SampleRsemGeneOutput(
  gene_id: String = "",
  sample_id: String = "",
  @ApiModelProperty(
    dataType = "double",
    required = false) length: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) effective_length: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) expected_count: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) tpm: Option[Double] = None,
  @ApiModelProperty(
    dataType = "double",
    required = false) fpkm: Option[Double] = None)

object SampleRsemGeneOutput {
  def readJson(gene_id: String,
               obj: JsObject,
               projectionFileds: ObjectFilters = new SampleRsemGeneProjectons()): JsResult[SampleRsemGeneOutput] = {

    val projectionFiledsObj = projectionFileds.asInstanceOf[SampleRsemGeneProjectons]
    JsSuccess(SampleRsemGeneOutput(
      gene_id = gene_id,
      sample_id = (obj \ "sample_id").as[String],
      length = obj.parseDoubleOption("length", projectionFiledsObj.length),
      effective_length = obj.parseDoubleOption("effective_length", projectionFiledsObj.effective_length),
      expected_count = obj.parseDoubleOption("expected_count", projectionFiledsObj.expected_count),
      tpm = obj.parseDoubleOption("tpm", projectionFiledsObj.tpm),
      fpkm = obj.parseDoubleOption("fpkm", projectionFiledsObj.fpkm)))

  }

  implicit val writeJson = new Writes[SampleRsemGeneOutput] {
    def writes(obj: SampleRsemGeneOutput): JsValue = {
      JsObjectWithOption("length" -> Right(obj.length.map(x => Json.toJson(x))),
        "effective_length" -> Right(obj.effective_length.map(x => Json.toJson(x))),
        "expected_count" -> Right(obj.expected_count.map(x => Json.toJson(x))),
        "tpm" -> Right(obj.tpm.map(x => Json.toJson(x))),
        "fpkm" -> Right(obj.fpkm.map(x => Json.toJson(x))))
    }
  }

  def getHeader(projection: Projection) = projection match {
    case Projection.detailed => Seq("length",
      "effective_length",
      "expected_count",
      "tpm",
      "fpkm")
    case Projection.summary => Seq("fpkm")
  }

  def getValues(obj: SampleRsemGeneOutput,
                projection: Projection) = projection match {
    case Projection.detailed => Seq(obj.length.getOrElse(""),
      obj.effective_length.getOrElse(""),
      obj.expected_count.getOrElse(""),
      obj.tpm.getOrElse(""),
      obj.fpkm.getOrElse(""))
    case Projection.summary => Seq(obj.fpkm.getOrElse(""))
  }
}
@ApiModel("TranscriptData")
case class TranscriptLevelOutput(
  transcript_id: String,
  @ApiModelProperty(
    name = "SampleAbundanceData",
    dataType = "de.v2.model.SampleAbundanceOutput",
    required = false) sample_abundance: Option[SampleAbundanceOutput],
  @ApiModelProperty(
    name = "SampleRsemIsoformData",
    dataType = "de.v2.model.SampleRsemIsoformOutput",
    required = false) sample_rsem_isoform: Option[SampleRsemIsoformOutput])

object TranscriptLevelOutput {

  implicit val WriteJson = Json.writes[TranscriptLevelOutput]

  def getValues(obj: TranscriptLevelOutput,
                norms: Seq[Normalization],
                projection: Projection) = {
    val sample_abundance_values = obj.sample_abundance match {
      case Some(x) => SampleAbundanceOutput.getValues(x, projection)
      case _ => {
        if (norms.contains(Normalization.sample_abundance))
          SampleAbundanceOutput.getValues(SampleAbundanceOutput(), projection)
        else
          Seq()
      }
    }

    val sample_rsem_isoform_values = obj.sample_rsem_isoform match {
      case Some(x) => SampleRsemIsoformOutput.getValues(x, projection)
      case _ => {
        if (norms.contains(Normalization.rsem))
          SampleRsemIsoformOutput.getValues(SampleRsemIsoformOutput(), projection)
        else
          Seq()
      }
    }
    Seq(obj.transcript_id) ++ sample_abundance_values ++ sample_rsem_isoform_values

  }
}

@ApiModel("SampleData")
case class SampleDataOutput(
  sample_id: String,

  @ApiModelProperty(
    name = "SampleRsemGeneData",
    dataType = "de.v2.model.SampleRsemGeneOutput",
    required = false) rsem: Option[SampleRsemGeneOutput],

  @ApiModelProperty(
    name = "TranscriptData",
    dataType = "de.v2.model.TranscriptLevelOutput",
    required = false) transcripts: Option[Seq[TranscriptLevelOutput]])

object SampleDataOutput {

  implicit val WriteJson = Json.writes[SampleDataOutput]

  def getValues(obj: SampleDataOutput, norms: Seq[Normalization], projection: Projection) = {
    val sample_rsem_values = obj.rsem match {
      case Some(x) => SampleRsemGeneOutput.getValues(x, projection)
      case _ => {
        if (norms.contains(Normalization.rsem))
          SampleRsemGeneOutput.getValues(SampleRsemGeneOutput(), projection)
        else
          Seq()
      }
    }

    val sample_rsem_values_with_sample_id = Seq(obj.sample_id) ++ sample_rsem_values

    obj.transcripts match {
      case Some(transcripts) => transcripts.map { transcript =>
        sample_rsem_values_with_sample_id ++ TranscriptLevelOutput
          .getValues(transcript, norms, projection)
      }
      case _ => Seq(sample_rsem_values_with_sample_id)
    }

  }
}

@ApiModel("GeneData")
case class GeneLevelOutput(
  gene_id: String,
  gene_symbol: String,
  @ApiModelProperty(
    name = "SampleData") data: Seq[SampleDataOutput])

object GeneLevelOutput {
  implicit val WriteJson = Json.writes[GeneLevelOutput]

  def getHeader(rsem: Boolean, sample_abundance: Boolean, sample_rsem_isoform: Boolean, norms: Seq[Normalization], projection: Projection) =
    Seq("gene_id", "gene_symbol", "data.sample_id") ++
      norms
      .map {
        _ match {
          case Normalization.rsem => SampleRsemGeneOutput
            .getHeader(projection)
            .map { x => s"""rsem.$x""" }
          case Normalization.sample_abundance => (Seq("transcript_id") ++
            SampleAbundanceOutput
            .getHeader(projection)
            .map { x => s"""sample_abundance.$x""" })
            .map { x => s"""transcripts.$x""" }
          case Normalization.sample_rsem_isoform => (Seq("transcript_id") ++
            SampleRsemIsoformOutput
            .getHeader(projection)
            .map { x => s"""sample_rsem_isoform.$x""" })
            .map { x => s"""transcripts.$x""" }
        }
      }
      .flatMap { _.map { x => s"""data.$x""" } }
      .distinct

  def getValues(obj: GeneLevelOutput, norms: Seq[Normalization], projection: Projection) = {
    obj.data.flatMap { x =>
      SampleDataOutput.getValues(x, norms, projection)
        .map { x => Seq(obj.gene_id, obj.gene_symbol) ++ x }
    }
  }
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
  def apply(obj: TranscriptInfoOutput,
            gene_id: String,
            gene_symbol: String): TranscriptWithGeneInfoOutput = {

    TranscriptWithGeneInfoOutput(obj.transcript_id,
      obj.start,
      obj.end,
      obj.biotype,
      obj.entrez_ids,
      obj.refseq_mrna_ids,
      obj.refseq_protein_ids,
      gene_id,
      gene_symbol)
  }
  implicit val WriteJson = Json.writes[TranscriptWithGeneInfoOutput]

  def getHeader = Seq("transcript_id",
    "start",
    "end",
    "biotype",
    "entrez_ids",
    "refseq_mrna_ids",
    "refseq_protein_ids",
    "gene_id",
    "gene_symbol")

  def getValues(obj: TranscriptWithGeneInfoOutput) =
    Seq(obj.transcript_id,
      obj.start,
      obj.end,
      obj.biotype,
      obj.entrez_ids.mkString(","),
      obj.refseq_mrna_ids.mkString(","),
      obj.refseq_protein_ids.mkString(","),
      obj.gene_id,
      obj.gene_symbol)

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

  def getHeader = Seq("transcript_id",
    "start",
    "end",
    "biotype",
    "entrez_ids",
    "refseq_mrna_ids",
    "refseq_protein_ids")

  def getValues(obj: TranscriptInfoOutput) = Seq(obj.transcript_id,
    obj.start,
    obj.end,
    obj.biotype,
    obj.entrez_ids.mkString(","),
    obj.refseq_mrna_ids.mkString(","),
    obj.refseq_protein_ids.mkString(","))
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

  def getHeader = Seq("gene_id",
    "gene_symbol",
    "start",
    "end",
    "biotype",
    "chr",
    "strand") ++ TranscriptInfoOutput.getHeader.map { x => s"""transcripts.$x""" }

  def getValues(obj: GeneInfoOutput) = {
    val _genedata = Seq(obj.gene_id,
      obj.gene_symbol,
      obj.start,
      obj.end,
      obj.biotype,
      obj.chr,
      obj.strand)
    obj.transcripts.map { transcript => _genedata ++ TranscriptInfoOutput.getValues(transcript) }
  }

}
