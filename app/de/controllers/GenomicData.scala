package de.controllers

import scala.{ Left, Right }

import de.Context
import de.model.Inputs._
import de.service.{ CassandraService, ElasticSearchService, MongoService }
import de.utils.{ InvalidQueryException, JsObjectWithOption, LoggingAction, SampleDataUtil }
import de.utils.Enums.{ Normalization, Projection }
import io.swagger.annotations.{ Api, ApiImplicitParams, ApiModel, ApiOperation, ApiParam }
import javax.inject.{ Inject, Singleton }
import play.api.http.HttpFilters
import play.api.libs.json.Json
import play.api.mvc.{ Accepting, Controller, RequestHeader }
import play.filters.gzip.GzipFilter
import io.swagger.annotations.ApiImplicitParam
import io.swagger.annotations.Example
import io.swagger.annotations.ExampleProperty
import de.model.Inputs.InputDataModel
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import de.model.GeneData

class Filters @Inject() (gzipFilter: GzipFilter) extends HttpFilters {
  def filters = Seq(gzipFilter)
}

@Api(value = "/Data",
  description = "Operations with Genes and Transcripts",
  produces = "application/json, text/tab-separated-values")
class GenomicData @javax.inject.Inject() (
  configuration: play.api.Configuration, context: Context)
    extends Controller {

  private val AcceptsTsv = Accepting("text/tab-separated-values")

  private val repo = context.getService.service

  /**
   * Converts gene data object to tsv format
   */
  private def tsvFormat(norms: Seq[Normalization], result: Seq[GeneData],
                        projection: Projection) = {
    val rsem = norms.contains(Normalization.rsem)
    val sample_abundance = norms.contains(Normalization.sample_abundance)
    val sample_rsem_isoform = norms.contains(Normalization.sample_rsem_isoform)
    val header = Seq(GeneData.getHeader(rsem, sample_abundance,
      sample_rsem_isoform, norms,
      projection).mkString("\t"))
    val data = result
      .map { geneData =>
        GeneData.getValues(geneData, norms, projection)
          .map { _.mkString("\t") }
          .mkString("\n")
      }
    (header ++ data).mkString("\n")
  }

  /**
   *
   *
   */
  private def getFields(projection: Projection,
                        normalizations: Seq[Normalization]): Map[Normalization, InputDataModel] =
    (projection match {
      case Projection.detailed => normalizations.map { x =>
        (x, x match {
          case Normalization.rsem => RsemGeneProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true,
            fpkm = true)
          case Normalization.sample_abundance => AbundanceProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true)
          case Normalization.sample_rsem_isoform => RsemIsoformProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true,
            fpkm = true,
            isoform_percentage = true)
        })
      }
      case Projection.summary => normalizations.map { normalization =>
        (normalization,
          normalization match {
            case Normalization.rsem => RsemGeneProjectons()
            case Normalization
              .sample_abundance => AbundanceProjectons()
            case Normalization
              .sample_rsem_isoform => RsemIsoformProjectons()
          })
      }
    })
      .toMap

  private def getData(filters: InputFilters,
                      ref_ids: Either[Seq[String], Seq[String]],
                      normalizations: Option[String],
                      projection: Option[String])(implicit request: RequestHeader) = {

    //map projection to enum
    val projection_enum = projection match {
      case Some(projection) => Projection.withNameOption(projection)
      case None             => Some(Projection.summary)
    }

    //map normalizarions to enum
    val normalization_enums = normalizations match {
      case Some(normalizations) => normalizations.split(",")
        .map(normalization => normalization -> Normalization
          .withNameOption(normalization)).toMap
      case None => Map(
        Normalization.rsem.entryName -> Some(Normalization
          .rsem),
        Normalization.sample_abundance
          .entryName -> Some(Normalization.sample_abundance),
        Normalization.sample_rsem_isoform
          .entryName -> Some(Normalization
            .sample_rsem_isoform))
    }

    val norm_invalid = normalization_enums.filter { case (_, norm_enum) => !norm_enum.isDefined }

    val proj_invalid = !projection_enum.isDefined

    if (norm_invalid.size > 0 || proj_invalid) {

      var obj = Json.obj()
      obj = if (norm_invalid.size > 0) {

        obj + ("normalizations" -> JsString(norm_invalid
          .keySet
          .mkString("", ",",
            " , are invalid. Must be in [rsem, sample_abundance, sample_rsem_isoform]")))
      } else Json.obj()

      obj = if (proj_invalid) {
        obj + ("projection" -> JsString(norm_invalid
          .keySet
          .mkString("", ",",
            " , are invalid. Must be in [rsem, sample_abundance, sample_rsem_isoform]")))
      } else Json.obj()

      BadRequest(obj)

    } else {

      val _projection_enum = projection_enum.get

      val _normalizations_enums = normalization_enums.values
        .map { _.get }
        .toSeq

      val start = System.currentTimeMillis()

      val _result = repo.getData(
        filters,
        getFields(_projection_enum,
          _normalizations_enums))

      println("Total Execution(ms) : " + (System.currentTimeMillis() - start))

      render {
        case Accepts.Json() => Ok(Json.toJson(_result))
        case AcceptsTsv() => Ok(tsvFormat(_normalizations_enums, _result,
          _projection_enum))
      }

    }
  }

  //START : data by gene Id
  @ApiOperation(value = "get all expression data for given gene entrez ids",
    notes = "Returns expression data for given gene entrez ids",
    response = classOf[GeneData],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIds(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {

    implicit request =>

      val filters = InputFilters(ref_id = Some(GeneIdQuery(gene_ids.split(",", -1).map { _.trim })))
      getData(
        filters,
        Left(Seq()),
        None,
        projection)

  }

  @ApiOperation(value = "get expression data for given gene entrez ids and studies",
    notes = "Returns expression data for given gene entrez ids and studies",
    response = classOf[GeneData],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndStudies(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(
        ref_id = Some(GeneIdQuery(gene_ids.split(",", -1).map { _.trim })),
        study_id = Some(studies_ids.split(",", -1).map { _.trim }))
      getData(
        filters,
        Left(studies_ids.split(",", -1)),
        None,
        projection)
  }

  @ApiOperation(value = "get expression data for given gene entrez ids",
    notes = "Returns expression data for given gene entrez ids",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      defaultValue = "rsem",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(ref_id = Some(GeneIdQuery(gene_ids.split(",", -1).map { _.trim })))
      getData(
        filters,
        Left(Seq()),
        Some(normalizations),
        projection)
  }

  @ApiOperation(value = "get expression data for given gene entrez ids and studies",
    notes = "Returns expression data for given gene entrez ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndStudiesAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(
        ref_id = Some(GeneIdQuery(gene_ids.split(",", -1).map { _.trim })),
        study_id = Some(studies_ids.split(",", -1).map { _.trim }))
      getData(
        filters,
        Left(studies_ids.split(",", -1)),
        Some(normalizations),
        projection)

  }

  //END : data by gene id
  // START : data by gene symbol
  @ApiOperation(value = "get expression data for given gene symbols",
    notes = "Returns expression data for given gene symbols",
    response = classOf[GeneData],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbols(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") gene_symbols: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(ref_id = Some(GeneSymbolQuery(gene_symbols.split(",", -1).map { _.trim })))
      getData(
        filters,
        Left(Seq()),
        None,
        projection)

  }

  @ApiOperation(value = "get expression data for given gene symbols and studies",
    notes = "Returns expression data for given gene symbols and studies",
    response = classOf[GeneData],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndStudies(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") gene_symbols: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(
        ref_id = Some(GeneSymbolQuery(gene_symbols.split(",", -1).map { _.trim })),
        study_id = Some(studies_ids.split(",", -1).map { _.trim }))
      getData(
        filters,
        Left(studies_ids.split(",", -1)),
        None,
        projection)
  }

  @ApiOperation(value = "get expression data for given gene symbols",
    notes = "Returns expression data for given gene symbols",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") gene_symbols: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(ref_id = Some(GeneSymbolQuery(gene_symbols.split(",", -1).map { _.trim })))
      getData(
        filters,
        Left(Seq()),
        Some(normalizations),
        projection)
  }

  @ApiOperation(value = "get expression data for given gene symbols and studies",
    notes = "Returns expression data for given gene symbols and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndStudiesAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") gene_symbols: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(
        ref_id = Some(GeneSymbolQuery(gene_symbols.split(",", -1).map { _.trim })),
        study_id = Some(studies_ids.split(",", -1).map { _.trim }))
      getData(
        filters,
        Left(studies_ids.split(",", -1)),
        Some(normalizations),
        projection)

  }

  //END : data by gene symbol
  // START : data by transcript id
  @ApiOperation(value = "get expression data for given transcript ids",
    notes = "Returns expression data for given transcript ids",
    response = classOf[GeneData],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIds(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") transcript_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(ref_id = Some(TranscriptIdQuery(transcript_ids.split(",", -1).map { _.trim })))
      getData(
        filters,
        Left(Seq()),
        None,
        projection)

  }

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[GeneData],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndStudies(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") transcript_ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(
        ref_id = Some(TranscriptIdQuery(transcript_ids.split(",", -1).map { _.trim })),
        study_id = Some(studies_ids.split(",", -1).map { _.trim }))
      getData(
        filters,
        Left(studies_ids.split(",", -1)),
        None,
        projection)
  }

  @ApiOperation(value = "get expression data for given transcript ids",
    notes = "Returns expression data for given transcript ids",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") transcript_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(ref_id = Some(TranscriptIdQuery(transcript_ids.split(",", -1).map { _.trim })))
      getData(
        filters,
        Left(Seq()),
        Some(normalizations),
        projection)
  }

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndStudiesAndNormalizations(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") transcript_ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      val filters = InputFilters(
        ref_id = Some(TranscriptIdQuery(transcript_ids.split(",", -1).map { _.trim })),
        study_id = Some(studies_ids.split(",", -1).map { _.trim }))
      getData(
        filters,
        Left(studies_ids.split(",", -1)),
        Some(normalizations),
        projection)

  }

  //END : data by transcript id

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "payload",
      required = true,
      value = """Its a JSON payload currently supports the following operations
                     &ensp; Logical Operators : $and, $not, $nor, $or
                     &ensp; Comparison Operators : $eq, $gt, $gte, $in, $lt, $lte, $ne, $nin
                {
                 &ensp;&ensp;"$and": [
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$eq": { "mycn_status":"amplified" }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$in": { "risk": ["high","low"] }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$not": { "$eq": { "stage": 4 }}},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$or": [
    	           &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "risk": "high" },
                 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "stage": 4 }]
                 &ensp;&ensp;&ensp;&ensp;&ensp;}]
                } """,
      paramType = "body",
      examples = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) //examples isn't working with this swaggger version
      )))
  def getDataByGeneSymbolsAndTagsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") gene_symbols: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction(bodyParser = parse.json) {
    implicit request =>

      val json = request.body
      try {
        val filters = InputFilters(
          ref_id = Some(GeneSymbolQuery(gene_symbols.split(",", -1).map { _.trim })),
          sample_id = Some(SampleDataUtil.getSamples(json).toSeq))
        getData(
          filters,
          Right(SampleDataUtil.getSamples(json).toSeq),
          Some(normalizations),
          projection)
      } catch {
        case x: InvalidQueryException => {
          BadRequest(x.getMessage)
        }
        case x: Throwable => {
          BadRequest("Unknown Exception")
        }
      }

  }

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "payload",
      required = true,
      value = """Its a JSON payload currently supports the following operations
                     &ensp; Logical Operators : $and, $not, $nor, $or
                     &ensp; Comparison Operators : $eq, $gt, $gte, $in, $lt, $lte, $ne, $nin
                {
                 &ensp;&ensp;"$and": [
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$eq": { "mycn_status":"amplified" }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$in": { "risk": ["high","low"] }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$not": { "$eq": { "stage": 4 }}},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$or": [
    	           &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "risk": "high" },
                 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "stage": 4 }]
                 &ensp;&ensp;&ensp;&ensp;&ensp;}]
                } """,
      paramType = "body",
      examples = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) //examples isn't working with this swaggger version
      )))
  def getDataByGeneIdsAndTagsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction(bodyParser = parse.json) {
    implicit request =>

      val json = request.body
      try {
        val filters = InputFilters(
          ref_id = Some(GeneIdQuery(gene_ids.split(",", -1).map { _.trim })),
          sample_id = Some(SampleDataUtil.getSamples(json).toSeq))

        getData(
          filters,
          Right(SampleDataUtil.getSamples(json).toSeq),
          Some(normalizations),
          projection)
      } catch {
        case x: InvalidQueryException => {
          BadRequest(x.getMessage)
        }
        case x: Throwable => {
          BadRequest("Unknown Exception")
        }
      }

  }

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "payload",
      required = true,
      value = """Its a JSON payload currently supports the following operations
                     &ensp; Logical Operators : $and, $not, $nor, $or
                     &ensp; Comparison Operators : $eq, $gt, $gte, $in, $lt, $lte, $ne, $nin
                {
                 &ensp;&ensp;"$and": [
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$eq": { "mycn_status":"amplified" }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$in": { "risk": ["high","low"] }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$not": { "$eq": { "stage": 4 }}},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$or": [
    	           &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "risk": "high" },
                 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "stage": 4 }]
                 &ensp;&ensp;&ensp;&ensp;&ensp;}]
                } """,
      paramType = "body",
      examples = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) //examples isn't working with this swaggger version
      )))
  def getDataByTranscriptIdsAndTagsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") transcript_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction(bodyParser = parse.json) {
    implicit request =>

      val json = request.body
      try {
        val filters = InputFilters(
          ref_id = Some(TranscriptIdQuery(transcript_ids.split(",", -1).map { _.trim })),
          sample_id = Some(SampleDataUtil.getSamples(json).toSeq))
        getData(
          filters,
          Right(SampleDataUtil.getSamples(json).toSeq),
          Some(normalizations),
          projection)
      } catch {
        case x: InvalidQueryException => {
          BadRequest(x.getMessage)
        }
        case x: Throwable => {
          BadRequest("Unknown Exception")
        }
      }

  }

  @ApiOperation(value = "get expression data for given sample tags and normalizations",
    notes = "Returns expression data for given sample tags and normalizations",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "payload",
      required = true,
      value = """Its a JSON payload currently supports the following operations
                     &ensp; Logical Operators : $and, $not, $nor, $or
                     &ensp; Comparison Operators : $eq, $gt, $gte, $in, $lt, $lte, $ne, $nin
                {
                 &ensp;&ensp;"$and": [
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$eq": { "mycn_status":"amplified" }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$in": { "risk": ["high","low"] }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$not": { "$eq": { "stage": 4 }}},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$or": [
    	           &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "risk": "high" },
                 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "stage": 4 }]
                 &ensp;&ensp;&ensp;&ensp;&ensp;}]
                } """,
      paramType = "body",
      examples = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) //examples isn't working with this swaggger version
      )))
  def getDataByTagsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction(bodyParser = parse.json) {
    implicit request =>

      val json = request.body
      try {
        val filters = InputFilters(
          sample_id = Some(SampleDataUtil.getSamples(json).toSeq))
        println(SampleDataUtil.getSamples(json).toSeq)
        getData(
          filters,
          Right(SampleDataUtil.getSamples(json).toSeq),
          Some(normalizations),
          projection)
      } catch {
        case x: InvalidQueryException => {
          BadRequest(x.getMessage)
        }
        case x: Throwable => {
          BadRequest("Unknown Exception")
        }
      }

  }

}
