package de.model

import scala.Right
import io.swagger.annotations.ApiModel
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{ JsObject, JsResult, JsSuccess, JsValue, Json, Writes }
import play.api.libs.json.JsValue.jsValueToJsLookup

import de.model.Inputs._
import de.utils.JsObjectWithOption
import de.utils.PlayJsonUtils.JsObjectImplicits
import de.utils.Enums.Projection
import de.utils.Enums.Normalization
import com.datastax.driver.core.Row
import de.utils.NumberUtils.DoubleImplicits
import de.utils.Transcript

//initialized parameters with default values. This would be used in getting tsv format data
//when querying for a particular normalization and doesn't have any data in the database
@ApiModel("RsemIsoformData")
case class RsemIsoform(transcript_id: String = "",
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

object RsemIsoform {

  def readJson(transcript_id: String,
               obj: JsObject,
               projectionFileds: InputDataModel = new RsemIsoformProjectons): JsResult[RsemIsoform] = {
    val projectionFiledsObj = projectionFileds
      .asInstanceOf[RsemIsoformProjectons]
    JsSuccess(RsemIsoform(
      transcript_id = transcript_id,
      sample_id = (obj \ "sample_id")
        .as[String],
      length = obj
        .parseDoubleOption("length",
          projectionFiledsObj
            .length),
      effective_length = obj
        .parseDoubleOption("effective_length",
          projectionFiledsObj
            .effective_length),
      expected_count = obj
        .parseDoubleOption("expected_count",
          projectionFiledsObj
            .expected_count),
      tpm = obj
        .parseDoubleOption("tpm",
          projectionFiledsObj
            .tpm),
      fpkm = obj
        .parseDoubleOption("fpkm",
          projectionFiledsObj
            .fpkm),
      isoform_percentage = obj
        .parseDoubleOption("isoform_percentage",
          projectionFiledsObj
            .isoform_percentage)))

  }
  def readRow(row: Row,
              projectionFileds: InputDataModel = new RsemIsoformProjectons()): JsResult[RsemIsoform] = {

    val projectionFiledsObj = projectionFileds
      .asInstanceOf[RsemIsoformProjectons]
    JsSuccess(RsemIsoform(transcript_id = row.getString("transcript_id"),
      sample_id = row.getString("sample_id"),
      length = row
        .getFloat("length")
        .parseDoubleOption(projectionFiledsObj.length),
      effective_length = row
        .getFloat("effective_length")
        .parseDoubleOption(projectionFiledsObj.effective_length),
      expected_count = row
        .getFloat("expected_count")
        .parseDoubleOption(projectionFiledsObj.expected_count),
      tpm = row
        .getFloat("tpm")
        .parseDoubleOption(projectionFiledsObj.tpm),
      fpkm = row
        .getFloat("fpkm")
        .parseDoubleOption(projectionFiledsObj.fpkm),
      isoform_percentage = row
        .getFloat("isoform_percentage")
        .parseDoubleOption(projectionFiledsObj.isoform_percentage)))
  }

  implicit val writeJson = new Writes[RsemIsoform] {
    def writes(obj: RsemIsoform): JsValue = {
      JsObjectWithOption("length" -> Right(obj.length.map(Json.toJson(_))),
        "effective_length" -> Right(obj.effective_length
          .map(Json.toJson(_))),
        "expected_count" -> Right(obj.expected_count
          .map(Json.toJson(_))),
        "tpm" -> Right(obj.tpm.map(Json.toJson(_))),
        "fpkm" -> Right(obj.fpkm.map(Json.toJson(_))),
        "isoform_percentage" -> Right(obj.isoform_percentage
          .map(Json.toJson(_))))
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

  def getValues(obj: RsemIsoform,
                projection: Projection) = projection match {
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
@ApiModel("AbundanceData")
case class Abundance(
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

object Abundance {

  def readJson(transcript_id: String,
               obj: JsObject,
               projectionFileds: InputDataModel = new AbundanceProjectons): JsResult[Abundance] = {
    val projectionFiledsObj = projectionFileds
      .asInstanceOf[AbundanceProjectons]

    JsSuccess(Abundance(
      transcript_id = transcript_id,
      sample_id = (obj \ "sample_id").as[String],
      length = obj
        .parseDoubleOption("length",
          projectionFiledsObj
            .length),
      effective_length = obj
        .parseDoubleOption("effective_length",
          projectionFiledsObj
            .effective_length),
      expected_count = obj
        .parseDoubleOption("expected_count",
          projectionFiledsObj
            .expected_count),
      tpm = obj
        .parseDoubleOption("tpm",
          projectionFiledsObj
            .tpm)))
  }

  def readRow(row: Row,
              projectionFileds: InputDataModel = new AbundanceProjectons()): JsResult[Abundance] = {

    val projectionFiledsObj = projectionFileds
      .asInstanceOf[AbundanceProjectons]
    JsSuccess(Abundance(transcript_id = row.getString("transcript_id"),
      sample_id = row.getString("sample_id"),
      length = row
        .getFloat("length")
        .parseDoubleOption(projectionFiledsObj.length),
      effective_length = row
        .getFloat("effective_length")
        .parseDoubleOption(projectionFiledsObj.effective_length),
      expected_count = row
        .getFloat("expected_count")
        .parseDoubleOption(projectionFiledsObj.expected_count),
      tpm = row
        .getFloat("tpm")
        .parseDoubleOption(projectionFiledsObj.tpm)))
  }

  implicit val writeJson = new Writes[Abundance] {
    def writes(obj: Abundance): JsValue = {
      JsObjectWithOption("length" -> Right(obj.length.map(Json.toJson(_))),
        "effective_length" -> Right(obj.effective_length
          .map(Json.toJson(_))),
        "expected_count" -> Right(obj.expected_count
          .map(Json.toJson(_))),
        "tpm" -> Right(obj.tpm.map(Json.toJson(_))))
    }
  }

  def getHeader(projection: Projection) = projection match {
    case Projection.detailed => Seq("length",
      "effective_length",
      "expected_count",
      "tpm")
    case Projection.summary => Seq("tpm")
  }

  def getValues(obj: Abundance,
                projection: Projection) = projection match {
    case Projection.detailed => Seq(obj.length.getOrElse(""),
      obj.effective_length.getOrElse(""),
      obj.expected_count.getOrElse(""),
      obj.tpm.getOrElse(""))
    case Projection.summary => Seq(obj.tpm.getOrElse(""))
  }

}

//initialized parameters with default values. used would be used when getting tsv format data
@ApiModel("RsemGeneData")
case class RsemGene(
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

object RsemGene {
  def readJson(gene_id: String,
               obj: JsObject,
               projectionFileds: InputDataModel = new RsemGeneProjectons()): JsResult[RsemGene] = {

    val projectionFiledsObj = projectionFileds
      .asInstanceOf[RsemGeneProjectons]
    JsSuccess(RsemGene(gene_id = gene_id,
      sample_id = (obj \ "sample_id").as[String],
      length = obj
        .parseDoubleOption("length",
          projectionFiledsObj
            .length),
      effective_length = obj
        .parseDoubleOption("effective_length",
          projectionFiledsObj
            .effective_length),
      expected_count = obj
        .parseDoubleOption("expected_count",
          projectionFiledsObj
            .expected_count),
      tpm = obj
        .parseDoubleOption("tpm",
          projectionFiledsObj
            .tpm),
      fpkm = obj
        .parseDoubleOption("fpkm",
          projectionFiledsObj
            .fpkm)))

  }

  def readRow(row: Row,
              projectionFileds: InputDataModel = new RsemGeneProjectons()): JsResult[RsemGene] = {

    val projectionFiledsObj = projectionFileds
      .asInstanceOf[RsemGeneProjectons]
    JsSuccess(RsemGene(gene_id = row.getString("gene_id"),
      sample_id = row.getString("sample_id"),
      length = row
        .getFloat("length")
        .parseDoubleOption(projectionFiledsObj.length),
      effective_length = row
        .getFloat("effective_length")
        .parseDoubleOption(projectionFiledsObj.effective_length),
      expected_count = row
        .getFloat("expected_count")
        .parseDoubleOption(projectionFiledsObj.expected_count),
      tpm = row
        .getFloat("tpm")
        .parseDoubleOption(projectionFiledsObj.tpm),
      fpkm = row
        .getFloat("fpkm")
        .parseDoubleOption(projectionFiledsObj.fpkm)))
  }

  implicit val writeJson = new Writes[RsemGene] {
    def writes(obj: RsemGene): JsValue = {
      JsObjectWithOption("length" -> Right(obj.length.map(Json.toJson(_))),
        "effective_length" -> Right(obj.effective_length
          .map(Json.toJson(_))),
        "expected_count" -> Right(obj.expected_count
          .map(Json.toJson(_))),
        "tpm" -> Right(obj.tpm.map(Json.toJson(_))),
        "fpkm" -> Right(obj.fpkm.map(Json.toJson(_))))
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

  def getValues(obj: RsemGene,
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
case class TranscriptData(transcript_id: String,
                          @ApiModelProperty(
                            name = "SampleAbundanceData",
                            dataType = "de.model.Abundance",
                            required = false) sample_abundance: Option[Abundance],
                          @ApiModelProperty(
                            name = "RsemIsoformData",
                            dataType = "de.model.RsemIsoform",
                            required = false) sample_rsem_isoform: Option[RsemIsoform])

object TranscriptData {

  implicit val WriteJson = Json.writes[TranscriptData]

  def getValues(obj: TranscriptData,
                norms: Seq[Normalization],
                projection: Projection) = {
    val sample_abundance_values = obj.sample_abundance match {
      case Some(data) => Abundance.getValues(data, projection)
      case _ => {
        if (norms.contains(Normalization.sample_abundance))
          Abundance.getValues(Abundance(), projection)
        else
          Seq()
      }
    }

    val sample_rsem_isoform_values = obj.sample_rsem_isoform match {
      case Some(data) => RsemIsoform.getValues(data, projection)
      case _ => {
        if (norms.contains(Normalization.rsem))
          RsemIsoform
            .getValues(RsemIsoform(), projection)
        else
          Seq()
      }
    }
    Seq(obj
      .transcript_id) ++ sample_abundance_values ++ sample_rsem_isoform_values

  }
}

@ApiModel("SampleData")
case class SampleData(sample_id: String,

                      @ApiModelProperty(
                        name = "SampleRsemGeneData",
                        dataType = "de.model.RsemGene",
                        required = false) rsem: Option[RsemGene],

                      @ApiModelProperty(
                        name = "TranscriptData",
                        dataType = "de.model.TranscriptData",
                        required = false) transcripts: Option[Seq[TranscriptData]])

object SampleData {

  implicit val WriteJson = Json.writes[SampleData]

  def getValues(obj: SampleData,
                norms: Seq[Normalization],
                projection: Projection) = {
    val sample_rsem_values = obj.rsem match {
      case Some(data) => RsemGene.getValues(data, projection)
      case _ => {
        if (norms.contains(Normalization.rsem))
          RsemGene.getValues(RsemGene(), projection)
        else
          Seq()
      }
    }

    val sample_rsem_values_with_sample_id = Seq(obj
      .sample_id) ++ sample_rsem_values

    obj.transcripts match {
      case Some(transcripts) => transcripts.map { transcript =>
        sample_rsem_values_with_sample_id ++ TranscriptData
          .getValues(transcript, norms, projection)
      }
      case _ => Seq(sample_rsem_values_with_sample_id)
    }

  }
}

@ApiModel("GeneData")
case class GeneData(
  gene_id: String,
  gene_symbol: String,
  @ApiModelProperty(
    name = "SampleData") data: Seq[SampleData])

object GeneData {
  implicit val WriteJson = Json.writes[GeneData]

  def getHeader(rsem: Boolean,
                sample_abundance: Boolean,
                sample_rsem_isoform: Boolean,
                norms: Seq[Normalization],
                projection: Projection) =
    Seq("gene_id", "gene_symbol", "data.sample_id") ++
      norms
      .map {
        _ match {
          case Normalization.rsem => RsemGene
            .getHeader(projection)
            .map { "rsem." + _ }
          case Normalization
            .sample_abundance => (Seq("transcript_id") ++
            Abundance
            .getHeader(projection)
            .map { "sample_abundance." + _ })
            .map { "transcripts." + _ }
          case Normalization
            .sample_rsem_isoform => (Seq("transcript_id") ++
            RsemIsoform
            .getHeader(projection)
            .map { "sample_rsem_isoform." + _ })
            .map { "transcripts." + _ }
        }
      }
      .flatMap {
        _.map { "data." + _ }
      }
      .distinct

  def getValues(obj: GeneData,
                norms: Seq[Normalization],
                projection: Projection) = {
    obj.data.flatMap { sampleData =>
      SampleData.getValues(sampleData, norms, projection)
        .map { Seq(obj.gene_id, obj.gene_symbol) ++ _ }
    }
  }
}

@ApiModel("TranscriptWithGeneInfo")
case class TranscriptWithGeneInfo(
  transcript_id: String,
  start: Long,
  end: Long,
  biotype: String,
  entrez_ids: Option[Seq[String]],
  refseq_mrna_ids: Option[Seq[String]],
  refseq_protein_ids: Option[Seq[String]],
  gene_id: String,
  gene_symbol: String)

object TranscriptWithGeneInfo {
  def apply(obj: TranscriptInfo,
            gene_id: String,
            gene_symbol: String): TranscriptWithGeneInfo = {

    TranscriptWithGeneInfo(obj.transcript_id,
      obj.start,
      obj.end,
      obj.biotype,
      obj.entrez_ids,
      obj.refseq_mrna_ids,
      obj.refseq_protein_ids,
      gene_id,
      gene_symbol)
  }

  implicit val WriteJson = Json.writes[TranscriptWithGeneInfo]

  def getHeader = Seq("transcript_id",
    "start",
    "end",
    "biotype",
    "entrez_ids",
    "refseq_mrna_ids",
    "refseq_protein_ids",
    "gene_id",
    "gene_symbol")

  def getValues(obj: TranscriptWithGeneInfo) =
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
case class TranscriptInfo(transcript_id: String,
                          start: Long,
                          end: Long,
                          biotype: String,
                          entrez_ids: Option[Seq[String]],
                          refseq_mrna_ids: Option[Seq[String]],
                          refseq_protein_ids: Option[Seq[String]])

object TranscriptInfo {
  def apply(obj: Transcript): TranscriptInfo = {
    TranscriptInfo(
      obj.transcript_id,
      obj.transcript_start,
      obj.transcript_end,
      obj.transcript_biotype,
      obj.entrez_id,
      obj.refseq_mrna_id,
      obj.refseq_protein_id)
  }

  implicit val WriteJson = Json.writes[TranscriptInfo]

  def getHeader = Seq("transcript_id",
    "start",
    "end",
    "biotype",
    "entrez_ids",
    "refseq_mrna_ids",
    "refseq_protein_ids")

  def getValues(obj: TranscriptInfo) = Seq(obj.transcript_id,
    obj.start,
    obj.end,
    obj.biotype,
    obj.entrez_ids.mkString(","),
    obj.refseq_mrna_ids
      .mkString(","),
    obj.refseq_protein_ids
      .mkString(","))
}

@ApiModel(value = "Gene")
case class GeneInfo(gene_id: String,
                    gene_symbol: String,
                    start: Long,
                    end: Long,
                    biotype: String,
                    chr: String,
                    strand: String,
                    transcripts: Seq[TranscriptInfo])

object GeneInfo {
  def apply(geneTranscriptInfo: Seq[Transcript]): GeneInfo = {
    GeneInfo(geneTranscriptInfo(0).gene_id,
      geneTranscriptInfo(0).gene_symbol,
      geneTranscriptInfo(0).gene_start,
      geneTranscriptInfo(0).gene_end,
      geneTranscriptInfo(0).gene_biotype,
      geneTranscriptInfo(0).chr,
      geneTranscriptInfo(0).strand,
      geneTranscriptInfo.map {
        TranscriptInfo.apply
      })
  }

  implicit val WriteJson = Json.writes[GeneInfo]

  def getHeader = Seq("gene_id",
    "gene_symbol",
    "start",
    "end",
    "biotype",
    "chr",
    "strand") ++ TranscriptInfo.getHeader
    .map { "transcripts." + _ }

  def getValues(obj: GeneInfo) = {
    val _genedata = Seq(obj.gene_id,
      obj.gene_symbol,
      obj.start,
      obj.end,
      obj.biotype,
      obj.chr,
      obj.strand)
    obj.transcripts
      .map { transcript =>
        _genedata ++ TranscriptInfo
          .getValues(transcript)
      }
  }

}
