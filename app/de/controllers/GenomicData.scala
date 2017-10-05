package de.controllers

import javax.inject.Singleton
import play.api.Configuration
import play.api.libs.json.{JsObject, JsString, Json, JsValue}
import play.api.mvc.{ Accepting, Controller, RequestHeader, Result }
import io.swagger.annotations.{ Api, ApiImplicitParams, ApiImplicitParam, ApiModel, ApiOperation, ApiParam, Example, ExampleProperty }
import de.Context
import de.model.GeneData
import de.model.Inputs.{GeneIdQuery, GeneSymbolQuery, InputFilters, TranscriptIdQuery}
import de.utils.{InvalidQueryException, LoggingAction, SampleDataUtil}
import scala.{Left, Right}

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
          filters        = geneIdFilters(gene_ids),
          ref_ids        = Left(Seq()),
          normalizations = None,
          projection)  
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
          filters        = geneIdFilters(gene_ids, study_ids),
          ref_ids        = Left(splitCsv(study_ids)),
          normalizations = None,
          projection)
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
          filters        = geneIdFilters(gene_ids),
          ref_ids        = Left(Seq()),
          normalizations = Some(normalizations),
          projection)
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
          filters        = geneIdFilters(gene_ids, study_ids),
          ref_ids        = Left(splitCsv(study_ids)),
          normalizations = Some(normalizations),
          projection)  
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
          filters        = geneSymbolFilters(gene_symbols),
          ref_ids        = Left(Seq()),
          normalizations = None,
          projection)  
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
          filters        = geneSymbolFilters(gene_symbols, study_ids),
          ref_ids        = Left(splitCsv(study_ids)),
          normalizations = None,
          projection)
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
          filters        = geneSymbolFilters(gene_symbols),
          ref_ids        = Left(Seq()),
          Some(normalizations),
          projection)
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
          filters        = geneSymbolFilters(gene_symbols, study_ids),
          ref_ids        = Left(splitCsv(study_ids)),
          normalizations = Some(normalizations),
          projection)
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
          filters        = transcriptIdFilters(transcript_ids),
          ref_ids        = Left(Seq()),
          normalizations = None,
          projection)
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
        filters        = transcriptIdFilters(transcript_ids, study_ids),
        ref_ids        = Left(splitCsv(study_ids)),
        normalizations = None,
        projection)
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
          filters        = transcriptIdFilters(transcript_ids),
          ref_ids        = Left(Seq()),
          normalizations = Some(normalizations),
          projection)
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
          filters        = transcriptIdFilters(transcript_ids, study_ids),
          ref_ids        = Left(splitCsv(study_ids)),
          normalizations = Some(normalizations),
          projection)  
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
            filters        = geneSymbolFilters(gene_symbols)
                               .copy(sample_id = Some(extractSampleIds(json))),
            ref_ids        = Right(extractSampleIds(json)),
            normalizations = Some(normalizations),
            projection)
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
            filters        = geneIdFilters(gene_ids)
                               .copy(sample_id = Some(extractSampleIds(json))),
            ref_ids        = Right(extractSampleIds(json)),
            normalizations = Some(normalizations),
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
            filters        = transcriptIdFilters(transcript_ids)
                               .copy(sample_id = Some(extractSampleIds(json))),
            ref_ids        = Right(extractSampleIds(json)),
            normalizations = Some(normalizations),
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
            filters        = sampleIdFilters(json),
            ref_ids        = Right(extractSampleIds(json)),
            normalizations = Some(normalizations),
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
          
  // ===========================================================================  
  private def apply
      (filters:        InputFilters,
       ref_ids:        Either[Seq[String], Seq[String]],
       normalizations: Option[String],
       projection:     Option[String])
      (implicit request: RequestHeader)
    : Result =

    // TODO: earlier input validation
    
    GenomicDataHelper(
        repo, 
        filters,
        ref_ids,
        normalizations,
        projection)
      match {
      
        case Left(errorObject) =>
          BadRequest(errorObject)

        case Right(genes) =>          
          render {
              
            case Accepts.Json() =>
              Ok(Json.toJson(genes))
              
            case Play.AcceptsTsv() =>
              Ok(
                TsvFormatter.rnaSeq(
                  genes,
                  GenomicDataHelper.normalizations(normalizations),
                  GenomicDataHelper.projection_enum(projection).get))
          }          
      }  
  
  // ===========================================================================
  def extractSampleIds(json: JsValue): Seq[String] =
    SampleDataUtil.getSamples(json).toSeq

  // ---------------------------------------------------------------------------
  def splitCsv(csvIds: String): Seq[String] =
    csvIds
      .split(",", -1)
      .map(_.trim) // TODO: don't
  
  // ===========================================================================
  // utils
  
  // TODO: externalize
  
  def geneIdFilters(gene_ids: String): InputFilters =
    InputFilters(ref_id = Some(GeneIdQuery(splitCsv(gene_ids))) )

  // ---------------------------------------------------------------------------
  def geneIdFilters(gene_ids: String, study_ids: String): InputFilters =          
    geneIdFilters(gene_ids).copy(study_id = Some(splitCsv(study_ids)))

  // ---------------------------------------------------------------------------
  def geneSymbolFilters(gene_symbols: String): InputFilters =
    InputFilters(ref_id = Some(GeneSymbolQuery(splitCsv(gene_symbols))))

  // ---------------------------------------------------------------------------
  def geneSymbolFilters(gene_symbols: String, study_ids: String): InputFilters =
    geneSymbolFilters(gene_symbols).copy(study_id = Some(splitCsv(study_ids)))

  // ---------------------------------------------------------------------------
  def transcriptIdFilters(transcript_ids: String): InputFilters =
    InputFilters(ref_id = Some(TranscriptIdQuery(splitCsv(transcript_ids))))

  // ---------------------------------------------------------------------------
  def transcriptIdFilters(transcript_ids: String, study_ids: String): InputFilters =
    transcriptIdFilters(transcript_ids).copy(study_id = Some(splitCsv(study_ids)))  

  // ---------------------------------------------------------------------------
  def sampleIdFilters(json: JsValue): InputFilters =
    InputFilters(sample_id = Some(extractSampleIds(json)))
  
}

// ===========================================================================
