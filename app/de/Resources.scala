package de

import javax.inject.Inject
import play.api.Configuration
import play.api.libs.json.Json
import play.api.mvc.{ Action, Controller }
// ===========================================================================
class Resources @Inject() (playconfiguration: Configuration) extends Controller {

  private implicit val AppCtx = Context(playconfiguration)
  private val repository = new Repository

  def getFields(projection: Option[Boolean], normalizations: String = "rsem,sample_abundance,sample_rsem_isoform") = {
    projection match {
      case Some(true) => normalizations.split(",").map { x =>
        x match {
          case "rsem"                => Seq("samples.rsem.fpkm")
          case "sample_abundance"    => Seq("samples.transcripts.sample_abundance.tpm", "samples.transcripts.transcript_id")
          case "sample_rsem_isoform" => Seq("samples.transcripts.sample_rsem_isoform.tpm", "samples.transcripts.transcript_id")
          case _                     => Seq()
        }
      }.flatten :+ ("samples.sample_id")
      case None | Some(false) => normalizations.split(",").map { x =>
        x match {
          case "rsem"                => Seq("samples.rsem")
          case "sample_abundance"    => Seq("samples.transcripts.sample_abundance", "samples.transcripts.transcript_id")
          case "sample_rsem_isoform" => Seq("samples.transcripts.sample_rsem_isoform", "samples.transcripts.transcript_id")
          case _                     => Seq()
        }
      }.flatten :+ ("samples.sample_id")
    }

  }

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
    Ok(Json.toJson(GeneDataUtil.getGeneSymbols()))
  }
  def getSamples(studyIds: String) = Action {
    Ok(Json.toJson(SampleDataUtil.getSamples(studyIds.split(","))))
  }
  def getAllSamples() = Action {
    Ok(Json.toJson(SampleDataUtil.getSamples()))
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

  def getDataByGeneIds(gene_ids: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("_id" -> gene_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          getFields(projection))))
  }

  def getDataByGeneIdsAndNormalizations(gene_ids: String, normalizations: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("_id" -> gene_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          getFields(projection, normalizations))))
  }

  def getDataByGeneIdsAndStudies(genes_ids: String, studies_ids: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("_id" -> genes_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          getFields(projection))))

  }

  def getDataByGeneIdsAndStudiesAndNormalizations(genes_ids: String, studies_ids: String, normalizations: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("_id" -> genes_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          getFields(projection, normalizations))))

  }
  //============================Using Gene Symbol--------------------------------------
  def getDataByGeneSymbols(gene_ids: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("gene_info.symbol" -> gene_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          getFields(projection))))
  }

  def getDataByGeneSymbolsAndNormalizations(gene_ids: String, normalizations: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("gene_info.symbol" -> gene_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          getFields(projection, normalizations))))
  }

  def getDataByGeneSymbolsAndStudies(genes_ids: String, studies_ids: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("gene_info.symbol" -> genes_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          getFields(projection))))

  }

  def getDataByGeneSymbolsAndStudiesAndNormalizations(genes_ids: String, studies_ids: String, normalizations: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredSamples(
          Map("gene_info.symbol" -> genes_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          getFields(projection, normalizations))))

  }
  //============================Using Transcript level--------------------------------------
  def getDataByTranscriptIds(transcript_ids: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredTranscripts(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          transcript_ids.split(",").toSeq,
          getFields(projection))))
  }

  def getDataByTranscriptIdsAndNormalizations(transcript_ids: String, normalizations: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredTranscripts(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",").toSeq),
          SampleDataUtil.getSamples(),
          transcript_ids.split(",").toSeq,
          getFields(projection, normalizations))))
  }

  def getDataByTranscriptIdsAndStudies(transcript_ids: String, studies_ids: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredTranscripts(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          transcript_ids.split(",").toSeq,
          getFields(projection))))

  }

  def getDataByTranscriptIdsAndStudiesAndNormalizations(transcript_ids: String, studies_ids: String, normalizations: String, projection: Option[Boolean] = Some(false)) = Action {
    Ok(Json.toJson(
      repository
        .getDataByFilteredTranscripts(
          Map("gene_info.transcripts.transcript_id" -> transcript_ids.split(",")),
          SampleDataUtil.getSamples(studies_ids.split(",")),
          transcript_ids.split(",").toSeq,
          getFields(projection, normalizations))))

  }
}
