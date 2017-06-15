package de.v2.controllers

import scala.Right
import scala.annotation.implicitNotFound
import scala.annotation.migration

import de.v2.utils.SampleDataUtil
import de.v2.utils.Enums.Normalization
import de.v2.utils.Enums.Projection
import de.v2.utils.JsObjectWithOption
import de.v2.MongoRepository
import de.v2.MongoRepositoryConfig
import io.swagger.annotations.Api
import io.swagger.annotations.ApiModel
import io.swagger.annotations.ApiModelProperty
import io.swagger.annotations.ApiOperation
import io.swagger.annotations.ApiParam
import javax.inject.Inject
import play.api.http.HttpFilters
import play.api.libs.json.Json
import play.api.mvc.Accepting
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.RequestHeader
import play.filters.gzip.GzipFilter
import de.v2.model.Inputs._
import de.v2.model.GeneLevelOutput

class Filters @Inject() (gzipFilter: GzipFilter) extends HttpFilters {
  def filters = Seq(gzipFilter)
}

@Api(value = "/Data",
  description = "Operations with Genes and Transcripts",
  produces = "application/json, text/tab-separated-values")
class GenomicData @javax.inject.Inject() (
  configuration: play.api.Configuration)
    extends Controller {

  private val AcceptsTsv = Accepting("text/tab-separated-values")

  private val uri = configuration.getString("database.mongo.uri").get
  private val database = configuration.getString("database.mongo.db").get
  private val repo = new MongoRepository(
    conf =
      new MongoRepositoryConfig(
        uri = uri,
        database))

  /**
   * Converts gene data object to tsv format
   */
  private def tsvFormat(result: Seq[GeneLevelOutput], projection: Projection) = {
    val header = Seq(result.head.getHeader(projection.entryName).mkString("\t"))
    val data = result
      .map {
        _.getValues(projection.entryName)
          .map { _.mkString("\t") }
          .mkString("\n")
      }
    (header ++ data).mkString("\n")
  }

  /**
   *
   *
   */
  private def getFields(projection: Projection, normalizations: Seq[Normalization]) = {
    projection match {
      case Projection.detailed => normalizations.map {
        _ match {
          case Normalization.rsem => SampleRsemGeneProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true,
            fpkm = true)
          case Normalization.sample_abundance => SampleAbundanceProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true)
          case Normalization.sample_rsem_isoform => SampleRsemIsoformProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true,
            fpkm = true,
            isoform_percentage = true)
        }
      }
      case Projection.summary => normalizations.map {
        _ match {
          case Normalization.rsem                => SampleRsemGeneProjectons()
          case Normalization.sample_abundance    => SampleAbundanceProjectons()
          case Normalization.sample_rsem_isoform => SampleRsemIsoformProjectons()
        }
      }
    }

  }

  private def getData(_ids_type: String,
                      _ids: String,
                      studies_ids: Option[String],
                      normalizations: Option[String],
                      projection: Option[String])(implicit request: RequestHeader) = {

    //map projection to enum
    val projection_enum = projection match {
      case Some(projection) => Projection.withNameOption(projection)
      case None             => Some(Projection.summary)
    }

    //map normalizarions to enum
    val normalization_enums = normalizations match {
      case Some(x) => x.split(",").map(normalization => normalization -> Normalization.withNameOption(normalization)).toMap
      case None => Map(
        Normalization.rsem.entryName -> Some(Normalization.rsem),
        Normalization.sample_abundance.entryName -> Some(Normalization.sample_abundance),
        Normalization.sample_rsem_isoform.entryName -> Some(Normalization.sample_rsem_isoform))
    }

    val norm_invalid = normalization_enums.filter { case (_, norm_enum) => !norm_enum.isDefined }

    val proj_invalid = !projection_enum.isDefined

    if (norm_invalid.size > 0 || proj_invalid) {
      val t1 = if (norm_invalid.size > 0) Some(norm_invalid.keySet.mkString("", ",", " , are invalid. Must be in [rsem, sample_abundance, sample_rsem_isoform]")) else None
      val t2 = if (proj_invalid) Some(s"""${projection.get}, is invalid. Must be summary or detailed """) else None
      BadRequest(JsObjectWithOption("projection" -> Right(t2.map(x => Json.toJson(x))), "normalizations" -> Right(t1.map(x => Json.toJson(x)))))

    } else {

      val _projection_enum = projection_enum.get
      val _normalizations_enums = normalization_enums.values.map { _.get }.toSeq
      val _studies = studies_ids match {
        case Some(studies) => studies.split(",").toSeq
        case None          => SampleDataUtil.getStudies()
      }
      val _result = _ids_type match {
        case "gene_id" => repo.getDataByGeneIds(
          _ids.split(",").toSeq,
          getFields(projection = _projection_enum, normalizations = _normalizations_enums),
          _studies)
        case "gene_symbol" => repo.getDataByGeneSymbols(
          _ids.split(",").toSeq,
          getFields(projection = _projection_enum, normalizations = _normalizations_enums),
          _studies)
        case "transcript_id" => repo.getDataByTranscriptIds(
          _ids.split(",").toSeq,
          getFields(projection = _projection_enum, normalizations = _normalizations_enums),
          _studies)
      }

      render {
        case Accepts.Json() => Ok(Json.toJson(_result))
        case AcceptsTsv()   => Ok(tsvFormat(_result, _projection_enum))
      }

    }
  }
  //START : data by gene Id
  @ApiOperation(value = "get all expression data for given gene entrez ids",
    notes = "Returns expression data for given gene entrez ids",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIds(
    @ApiParam(value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("gene_id", gene_ids, None, None, projection)

  }

  @ApiOperation(value = "get expression data for given gene entrez ids and studies",
    notes = "Returns expression data for given gene entrez ids and studies",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndStudies(
    @ApiParam(value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("gene_id", gene_ids, Some(studies_ids), None, projection)
  }

  @ApiOperation(value = "get expression data for given gene entrez ids",
    notes = "Returns expression data for given gene entrez ids",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndNormalizations(
    @ApiParam(value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(value = "Comma separated list of normalization methods", allowableValues = "rsem,sample_abundance,sample_rsem_isoform", allowMultiple = true) normalizations: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("gene_id", gene_ids, None, Some(normalizations), projection)
  }

  @ApiOperation(value = "get expression data for given gene entrez ids and studies",
    notes = "Returns expression data for given gene entrez ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndStudiesAndNormalizations(
    @ApiParam(value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(value = "Comma separated list of normalization methods", allowableValues = "rsem,sample_abundance,sample_rsem_isoform", allowMultiple = true) normalizations: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("gene_id", gene_ids, Some(studies_ids), Some(normalizations), projection)

  }

  //END : data by gene id
  // START : data by gene symbol
  @ApiOperation(value = "get expression data for given gene symbols",
    notes = "Returns expression data for given gene symbols",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbols(
    @ApiParam(value = "Comma separated list of gene symbols. e.g. MYCN,TP53") gene_ids: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("gene_symbol", gene_ids, None, None, projection)

  }

  @ApiOperation(value = "get expression data for given gene symbols and studies",
    notes = "Returns expression data for given gene symbols and studies",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndStudies(
    @ApiParam(value = "Comma separated list of gene symbols. e.g. MYCN,TP53") _ids: String,
    @ApiParam(value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("gene_symbol", _ids, Some(studies_ids), None, projection)
  }

  @ApiOperation(value = "get expression data for given gene symbols",
    notes = "Returns expression data for given gene symbols",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndNormalizations(
    @ApiParam(value = "Comma separated list of gene symbols. e.g. MYCN,TP53") _ids: String,
    @ApiParam(value = "Comma separated list of normalization methods", allowableValues = "rsem,sample_abundance,sample_rsem_isoform", allowMultiple = true) normalizations: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("gene_symbol", _ids, None, Some(normalizations), projection)
  }

  @ApiOperation(value = "get expression data for given gene symbols and studies",
    notes = "Returns expression data for given gene symbols and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndStudiesAndNormalizations(
    @ApiParam(value = "Comma separated list of gene symbols. e.g. MYCN,TP53") _ids: String,
    @ApiParam(value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(value = "Comma separated list of normalization methods", allowableValues = "rsem,sample_abundance,sample_rsem_isoform", allowMultiple = true) normalizations: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("gene_symbol", _ids, Some(studies_ids), Some(normalizations), projection)

  }
  //END : data by gene symbol
  // START : data by transcript id
  @ApiOperation(value = "get expression data for given transcript ids",
    notes = "Returns expression data for given transcript ids",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIds(
    @ApiParam(value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") gene_ids: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("transcript_id", gene_ids, None, None, projection)

  }

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndStudies(
    @ApiParam(value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") _ids: String,
    @ApiParam(value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("transcript_id", _ids, Some(studies_ids), None, projection)
  }

  @ApiOperation(value = "get expression data for given transcript ids",
    notes = "Returns expression data for given transcript ids",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndNormalizations(
    @ApiParam(value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") _ids: String,
    @ApiParam(value = "Comma separated list of normalization methods", allowableValues = "rsem,sample_abundance,sample_rsem_isoform", allowMultiple = true) normalizations: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("transcript_id", _ids, None, Some(normalizations), projection)
  }

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndStudiesAndNormalizations(
    @ApiParam(value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") _ids: String,
    @ApiParam(value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(value = "Comma separated list of normalization methods", allowableValues = "rsem,sample_abundance,sample_rsem_isoform", allowMultiple = true) normalizations: String,
    @ApiParam(value = "Projection type summary or detailed", allowableValues = "summary,detailed", defaultValue = "summary") projection: Option[String]) = Action {
    implicit request =>
      getData("transcript_id", _ids, Some(studies_ids), Some(normalizations), projection)

  }
  //END : data by transcript id

}