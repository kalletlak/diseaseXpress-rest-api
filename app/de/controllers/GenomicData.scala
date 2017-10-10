package de.controllers

import javax.inject.Singleton
import play.api.Configuration
import play.api.libs.json.{JsObject, JsString, Json, JsValue}
import play.api.mvc.{ Accepting, Controller, RequestHeader, Result }
import io.swagger.annotations.{ Api, ApiImplicitParams, ApiImplicitParam, ApiModel, ApiOperation, ApiParam, Example, ExampleProperty }
import de.Context
import de.model.output.GeneData
import de.validators.{ GeneIdQuery, GeneSymbolQuery, TranscriptIdQuery, SampleQuery, StudyQuery}
import de.utils.{ InvalidQueryException, LoggingAction }
import de.validators.{ PrimaryIdsValidator, SecondaryIdsValidator, SecondaryIdRef }
import de.utils.Enums.Projection
import de.model.Error
import de.model.input.InputFilters
import de.repository.SamplesRepository

// ===========================================================================
@Api(
  value       = "/Data",
  description = "Operations with Genes and Transcripts",
  produces    = "application/json, text/tab-separated-values")
class GenomicData @javax.inject.Inject() (
      configuration: Configuration,
      context:       Context)
    extends Controller {

  
  // mostly boilerplate code in this class (TODO: generate from specification instead)
  
  
  private val repo = context.getService.service
  
  // ===========================================================================
  // by gene ID
  
  @ApiOperation(
    value             = "get all expression data for given gene entrez ids",
    notes             = "Returns expression data for given gene entrez ids",
    response          =  classOf[GeneData],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByGeneIds(

      @ApiParam("Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14")
      gene_ids: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =

    LoggingAction {
      implicit request =>          
        apply(
            primaryObject=GeneIdQuery,
            primaryIds=Some(gene_ids),
            projection=projection)  
      }

  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given gene entrez ids and studies",
    notes             = "Returns expression data for given gene entrez ids and studies",
    response          =  classOf[GeneData],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByGeneIdsAndStudies(
      
      @ApiParam("Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14")
      gene_ids: String,
      
      @ApiParam("Comma separated list of study ids. e.g. PNOC,TARGET")
      study_ids: String,

      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =

    LoggingAction {
      implicit request =>
        apply(
              primaryObject=GeneIdQuery,
              primaryIds=Some(gene_ids),
              secondaryObject=StudyQuery,
              secondaryIds=Some(study_ids),
              projection=projection)
      }

  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given gene entrez ids",
    notes             = "Returns expression data for given gene entrez ids",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByGeneIdsAndNormalizations(

      @ApiParam("Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14")
      gene_ids: String,
      
      @ApiParam(
          value           = "Comma separated list of normalization methods",
          allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
          defaultValue    = "rsem",
          allowMultiple   = true)
        normalizations: String,
  
      @ApiParam(
          value           = "Projection type summary or detailed",
          allowableValues = "summary,detailed",
          defaultValue    = "summary")
      projection: Option[String]) =

    LoggingAction {
      implicit request =>        
        apply(
              primaryObject=GeneIdQuery,
              primaryIds=Some(gene_ids),
              normalizations = Some(normalizations),
              projection=projection)
      }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given gene entrez ids and studies",
    notes             = "Returns expression data for given gene entrez ids and studies",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByGeneIdsAndStudiesAndNormalizations(
      
      @ApiParam("Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14")
      gene_ids: String,
      
      @ApiParam("Comma separated list of study ids. e.g. PNOC,TARGET")
      study_ids: String,

      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =
        
    LoggingAction {    
      implicit request =>      
        apply(
              primaryObject=GeneIdQuery,
              primaryIds=Some(gene_ids),
              secondaryObject=StudyQuery,
              secondaryIds=Some(study_ids),
              normalizations = Some(normalizations),
              projection=projection)  
      }
  
  // ===========================================================================
  // by gene symbol

  @ApiOperation(
    value             = "get expression data for given gene symbols",
    notes             = "Returns expression data for given gene symbols",
    response          =  classOf[GeneData],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByGeneSymbols(
      
      @ApiParam("Comma separated list of gene symbols. e.g. MYCN,TP53")
      gene_symbols: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =
        
    LoggingAction {
      implicit request =>      
        apply(
              primaryObject=GeneSymbolQuery,
              primaryIds=Some(gene_symbols),
              projection=projection)  
      }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given gene symbols and studies",
    notes             = "Returns expression data for given gene symbols and studies",
    response          =  classOf[GeneData],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByGeneSymbolsAndStudies(
      
      @ApiParam("Comma separated list of gene symbols. e.g. MYCN,TP53")
      gene_symbols: String,
      
      @ApiParam("Comma separated list of study ids. e.g. PNOC,TARGET")
      study_ids: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =
        
    LoggingAction {    
      implicit request =>
        apply(
              primaryObject=GeneSymbolQuery,
              primaryIds=Some(gene_symbols),
              secondaryObject=StudyQuery,
              secondaryIds=Some(study_ids),
              projection=projection)
      }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given gene symbols",
    notes             = "Returns expression data for given gene symbols",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByGeneSymbolsAndNormalizations(
      
      @ApiParam("Comma separated list of gene symbols. e.g. MYCN,TP53")
      gene_symbols: String,
      
      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =
        
    LoggingAction {    
      implicit request =>      
        apply(
              primaryObject=GeneSymbolQuery,
              primaryIds=Some(gene_symbols),
              normalizations = Some(normalizations),
              projection=projection)
      }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given gene symbols and studies",
    notes             = "Returns expression data for given gene symbols and studies",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByGeneSymbolsAndStudiesAndNormalizations(
      
      @ApiParam("Comma separated list of gene symbols. e.g. MYCN,TP53")
      gene_symbols: String,
      
      @ApiParam("Comma separated list of study ids. e.g. PNOC,TARGET")
      study_ids: String,
      
      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
        
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =

    LoggingAction {
      implicit request =>
        apply(
              primaryObject=GeneSymbolQuery,
              primaryIds=Some(gene_symbols),
              secondaryObject=StudyQuery,
              secondaryIds=Some(study_ids),
              normalizations = Some(normalizations),
              projection=projection)
    }
  

  // ===========================================================================
  // by transcript ID
  
  @ApiOperation(
    value             = "get expression data for given transcript ids",
    notes             = "Returns expression data for given transcript ids",
    response          =  classOf[GeneData],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByTranscriptIds(
      @ApiParam("Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2")
      transcript_ids: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =
      
    LoggingAction {    
      implicit request =>      
        apply(
              primaryObject=TranscriptIdQuery,
              primaryIds=Some(transcript_ids),
              projection=projection)
      }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given transcript ids and studies",
    notes             = "Returns expression data for given transcript ids and studies",
    response          =  classOf[GeneData],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByTranscriptIdsAndStudies(
      
      @ApiParam("Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2")
      transcript_ids: String,
      
      @ApiParam("Comma separated list of study ids. e.g. PNOC,TARGET")
      study_ids: String,
      
      @ApiParam(
        value             = "Projection type summary or detailed",
        allowableValues   = "summary,detailed",
        defaultValue      = "summary")
      projection: Option[String]) =
        
    LoggingAction {
    implicit request =>
      apply(
            primaryObject=TranscriptIdQuery,
            primaryIds=Some(transcript_ids),
            secondaryObject=StudyQuery,
            secondaryIds=Some(study_ids),
            projection=projection)
  }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given transcript ids",
    notes             = "Returns expression data for given transcript ids",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByTranscriptIdsAndNormalizations(
      
      @ApiParam("Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2")
      transcript_ids: String,
      
      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
        
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) = 
        
    LoggingAction {
      implicit request =>
        apply(
              primaryObject=TranscriptIdQuery,
              primaryIds=Some(transcript_ids),
              normalizations = Some(normalizations),
              projection=projection)
      }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given transcript ids and studies",
    notes             = "Returns expression data for given transcript ids and studies",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "GET")
  def getByTranscriptIdsAndStudiesAndNormalizations(
      
      @ApiParam("Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2")
      transcript_ids: String,
      
      @ApiParam("Comma separated list of study ids. e.g. PNOC,TARGET")
      study_ids: String,
      
      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =

    LoggingAction {
      implicit request =>
        apply(
              primaryObject=TranscriptIdQuery,
              primaryIds=Some(transcript_ids),
              secondaryObject=StudyQuery,
              secondaryIds=Some(study_ids),
              normalizations = Some(normalizations),
              projection=projection)  
      }

  
  // ===========================================================================
  // ??

  @ApiOperation(
    value             = "get expression data for given transcript ids and studies",
    notes             = "Returns expression data for given transcript ids and studies",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name     = "payload",
      required = true,
      value    =
        """Its a JSON payload currently supports the following operations
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
      examples  = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) //examples isn't working with this swaggger version
      )))
  def getByGeneSymbolsAndTagsAndNormalizations(
      
      @ApiParam("Comma separated list of gene symbols. e.g. MYCN,TP53")
      gene_symbols: String,
      
      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =
      
    LoggingAction(bodyParser = parse.json) {
    
      implicit request =>
  
        val json: JsValue =
          request.body
  
        try {
          apply(
              primaryObject=GeneSymbolQuery,
              primaryIds=Some(gene_symbols),
              secondaryObject=SampleQuery,
              secondaryIds=Some(extractSampleIds(json).mkString(",")),
              normalizations = Some(normalizations),
              projection=projection)
        } catch { // TODO: address properly instead
          case x: InvalidQueryException => {
            BadRequest(x.getMessage)
          }
          case x: Throwable => {
            BadRequest("Unknown Exception")
          }
        }  
      }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given transcript ids and studies",
    notes             = "Returns expression data for given transcript ids and studies",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name     = "payload",
      required = true,
      value    =
        """Its a JSON payload currently supports the following operations
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
      examples  = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) //examples isn't working with this swaggger version
      )))
  def getByGeneIdsAndTagsAndNormalizations(
      
      @ApiParam("Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14")
      gene_ids: String,
      
      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
      
      @ApiParam(
        value             = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue = "summary")
      projection: Option[String]) =
        
    LoggingAction(bodyParser = parse.json) {
      implicit request =>
  
        val json = request.body
        try {
          apply(
              primaryObject=GeneIdQuery,
              primaryIds=Some(gene_ids),
              secondaryObject=SampleQuery,
              secondaryIds=Some(extractSampleIds(json).mkString(",")),
              normalizations = Some(normalizations),
              projection=projection)
        } catch {
          case x: InvalidQueryException => {
            BadRequest(x.getMessage)
          }
          case x: Throwable => {
            BadRequest("Unknown Exception")
          }
        }  
      }

  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get expression data for given transcript ids and studies",
    notes             = "Returns expression data for given transcript ids and studies",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name     = "payload",
      required = true,
      value    =
        """Its a JSON payload currently supports the following operations
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
      examples  = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) //examples isn't working with this swaggger version
      )))
  def getByTranscriptIdsAndTagsAndNormalizations(
      
      @ApiParam("Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2")
      transcript_ids: String,
      
      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =

    LoggingAction(bodyParser = parse.json) {
      implicit request =>
  
        val json = request.body
        try {
          apply(
              primaryObject=TranscriptIdQuery,
              primaryIds=Some(transcript_ids),
              secondaryObject=SampleQuery,
              secondaryIds=Some(extractSampleIds(json).mkString(",")),
              normalizations = Some(normalizations),
              projection=projection)
        } catch {
          case x: InvalidQueryException => {
            BadRequest(x.getMessage)
          }
          case x: Throwable => {
            BadRequest("Unknown Exception")
          }
        }  
      }

  
  // ===========================================================================
  // by sample ID

  @ApiOperation(
    value             = "get expression data for given sample tags and normalizations",
    notes             = "Returns expression data for given sample tags and normalizations",
    response          =  classOf[String],
    responseContainer = "List",
    httpMethod        = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name     = "payload",
        required = true,
        value    =
          """Its a JSON payload currently supports the following operations
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
        examples  = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) // examples isn't working with this swaggger version
    )))
  def getByTagsAndNormalizations(

      @ApiParam(
        value           = "Comma separated list of normalization methods",
        allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
        allowMultiple   = true)
      normalizations: String,
      
      @ApiParam(
        value           = "Projection type summary or detailed",
        allowableValues = "summary,detailed",
        defaultValue    = "summary")
      projection: Option[String]) =

    LoggingAction(bodyParser = parse.json) {
      implicit request =>
  
        val json: JsValue =
          request.body
  
        // TODO: address properly
        try {
          println(extractSampleIds(json))
          apply(
              secondaryObject=SampleQuery,
              secondaryIds=Some(extractSampleIds(json).mkString(",")),
              normalizations = Some(normalizations),
              projection=projection)
        } catch {
          case x: InvalidQueryException => {
            BadRequest(x.getMessage)
          }
          case x: Throwable => {
            BadRequest("Unknown Exception")
          }
        }  
      }
          
  // ===========================================================================  
   private def apply(
                    primaryObject: PrimaryIdsValidator=GeneIdQuery,
                    secondaryObject: SecondaryIdsValidator=StudyQuery,
                    primaryIds: Option[String] = None,
                    secondaryIds: Option[String] = None,
                    normalizations: Option[String] = None,
                    projection: Option[String] = None)
                   (implicit request: RequestHeader): Result = {

      val input: Either[Seq[Error], InputFilters] = InputFilters(
                                                                primaryObject,
                                                                secondaryObject,
                                                                primaryIds,
                                                                secondaryIds,
                                                                normalizations,
                                                                projection)

      input match {
        case Left(errorObject) =>
          BadRequest(Json.toJson(errorObject))

        case Right(filters) => {
          val genes = repo
                        .getData(filters)
          render {
  
            case Accepts.Json() =>
              Ok(Json.toJson(genes))
  
            case Play.AcceptsTsv() => {
              val _projection = projection match {
                                  case Some(projection) => Projection.withNameOption(projection)
                                  case None             => Some(Projection.summary)
                                }
  
               Ok(
                  TsvFormatter.rnaSeq(
                    genes,
                    filters.normalization_combo.keySet.toSeq,
                    _projection.get))
            }
        }  
      }
    }
  }

  
  // ===========================================================================
  def extractSampleIds(json: JsValue): Seq[String] =
    SamplesRepository.getSamples(json).toSeq

  // ---------------------------------------------------------------------------
  def splitCsv(csvIds: String): Seq[String] =
    csvIds
      .split(",", -1)
  
  // ===========================================================================
  // utils
  
}

// ===========================================================================
