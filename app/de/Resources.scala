package de

import javax.inject.Inject
import play.api.Play
import play.api.http.HttpFilters
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.Action
import play.api.mvc.Controller
import play.filters.cors.CORSFilter

class Filters @Inject() (corsFilter: CORSFilter) extends HttpFilters {
  def filters = Seq(corsFilter)
}

// ===========================================================================
object Resources extends Controller {

  //private implicit val AppCtx = Context(playconfiguration)
  private implicit val AppCtx = Context(playConf = Play.current.configuration)
  def getFields(projection: Option[String] = Some("SUMMARY"), normalizations: String = "rsem,sample_abundance,sample_rsem_isoform") = {
    projection match {
      case Some("DETAILED") => normalizations.split(",").map { x =>
        x match {
          case "rsem"                => Seq("samples.rsem")
          case "sample_abundance"    => Seq("samples.transcripts.sample_abundance", "samples.transcripts.transcript_id")
          case "sample_rsem_isoform" => Seq("samples.transcripts.sample_rsem_isoform", "samples.transcripts.transcript_id")
          case _                     => Seq()
        }
      }.flatten :+ ("samples.sample_id")
      case Some("SUMMARY") => normalizations.split(",").map { x =>
        x match {
          case "rsem"                => Seq("samples.rsem.fpkm")
          case "sample_abundance"    => Seq("samples.transcripts.sample_abundance.tpm", "samples.transcripts.transcript_id")
          case "sample_rsem_isoform" => Seq("samples.transcripts.sample_rsem_isoform.tpm", "samples.transcripts.transcript_id")
          case _                     => Seq()
        }
      }.flatten :+ ("samples.sample_id")

      //TODO : need to update this
      case _ => normalizations.split(",").map { x =>
        x match {
          case "rsem"                => Seq("samples.rsem.fpkm")
          case "sample_abundance"    => Seq("samples.transcripts.sample_abundance.tpm", "samples.transcripts.transcript_id")
          case "sample_rsem_isoform" => Seq("samples.transcripts.sample_rsem_isoform.tpm", "samples.transcripts.transcript_id")
          case _                     => Seq()
        }
      }.flatten :+ ("samples.sample_id")
    }

  }

  val repository = new Repository(AppCtx)

  def getStudies() = Action {
    Ok(Json.toJson(SampleDataUtil.getStudies()))
  }
  def getGenesByIds() = Action {
    Ok(Json.toJson(GeneDataUtil.getGeneIds()))

  }
  def getGenesBySymbol() = Action {
    Ok(Json.toJson(GeneDataUtil.getGeneSymbols()))
  }
  def getTranscriptIds() = Action {
    Ok(Json.toJson(GeneDataUtil.getTranscriptIds()))
  }

  def getSamples(studyIds: String) = Action {
    val response = SampleDataUtil.getSamplesInfo(studyIds.split(",")).map { x =>
      Json.obj(
        "sample_id" -> x.analysis_id,
        "patient_barcode" -> x.patient_barcode,
        "sample_barcode" -> x.sample_barcode,
        "group" -> x.group,
        "study" -> x.study,
        "disease" -> x.disease,
        "disease_name" -> x.disease_name,
        "disease_subtype" -> x.disease_subtype,
        "tissue" -> x.tissue,
        "definition" -> x.definition,
        "library_type" -> x.library_type,
        "platform" -> x.platform,
        "center" -> x.center)
    }
    Ok(Json.toJson(response))
  }
  def getAllSamples() = Action {
    val response = SampleDataUtil.getSamplesInfo().map { x =>
      Json.obj(
        "sample_id" -> x.analysis_id,
        "patient_barcode" -> x.patient_barcode,
        "sample_barcode" -> x.sample_barcode,
        "group" -> x.group,
        "study" -> x.study,
        "disease" -> x.disease,
        "disease_name" -> x.disease_name,
        "disease_subtype" -> x.disease_subtype,
        "tissue" -> x.tissue,
        "definition" -> x.definition,
        "library_type" -> x.library_type,
        "platform" -> x.platform,
        "center" -> x.center)
    }
    Ok(Json.toJson(response))
  }

  def getGeneInfoByIds(gene_ids: String) = Action {
    Ok(Json.toJson(
      repository
        .getData(
          Map("_id" -> gene_ids.split(",")),
          Seq("gene_info"))))
  }
  def getGeneInfoBySymbols(gene_symbols: String) = Action {
    Ok(Json.toJson(
      repository
        .getData(
          Map("gene_info.symbol" -> gene_symbols.split(",")),
          Seq("gene_info"))))
  }

  def getTranscriptInfo(transcript_ids: String) = Action {
    Ok(Json.toJson(
      repository
        .getData(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",")),
          Seq("gene_info.transcripts"))))
  }

  //============================Using Gene Id--------------------------------------

  def getDataByGeneIds(
    gene_ids: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("_id" -> gene_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          getFields(projection))))
  }

  def getDataByGeneIdsAndNormalizations(
    gene_ids: String,
    normalizations: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("_id" -> gene_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          getFields(projection, normalizations))))
  }

  def getDataByGeneIdsAndStudies(
    genes_ids: String,
    studies_ids: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("_id" -> genes_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          getFields(projection))))

  }

  def getDataByGeneIdsAndStudiesAndNormalizations(
    genes_ids: String,
    studies_ids: String,
    normalizations: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("_id" -> genes_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          getFields(projection, normalizations))))

  }
  //============================Using Gene Symbol--------------------------------------
  def getDataByGeneSymbols(
    gene_ids: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("gene_info.symbol" -> gene_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          getFields(projection))))
  }

  def getDataByGeneSymbolsAndNormalizations(
    gene_ids: String,
    normalizations: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("gene_info.symbol" -> gene_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          getFields(projection, normalizations))))
  }

  def getDataByGeneSymbolsAndStudies(
    genes_ids: String,
    studies_ids: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("gene_info.symbol" -> genes_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          getFields(projection))))

  }

  def getDataByGeneSymbolsAndStudiesAndNormalizations(
    genes_ids: String,
    studies_ids: String,
    normalizations: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("gene_info.symbol" -> genes_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          getFields(projection, normalizations))))

  }
  //============================Using Transcript level--------------------------------------
  def getDataByTranscriptIds(
    transcript_ids: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredTranscripts(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          transcript_ids.split(",").toSeq,
          getFields(projection))))
  }

  def getDataByTranscriptIdsAndNormalizations(
    transcript_ids: String,
    normalizations: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredTranscripts(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          transcript_ids.split(",").toSeq,
          getFields(projection, normalizations))))
  }

  def getDataByTranscriptIdsAndStudies(
    transcript_ids: String,
    studies_ids: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredTranscripts(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          transcript_ids.split(",").toSeq,
          getFields(projection))))

  }

  def getDataByTranscriptIdsAndStudiesAndNormalizations(
    transcript_ids: String,
    studies_ids: String,
    normalizations: String,
    projection: Option[String]) = Action {

    Ok(Json.toJson(
      repository
        .getDataByFilteredTranscripts(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          transcript_ids.split(",").toSeq,
          getFields(projection, normalizations))))

  }
}
