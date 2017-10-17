package de.controllers

import de.Context
import de.model.Error
import de.model.input.InputFilters
import de.utils.Enums.Projection
import de.utils.LoggingAction
import de.validators.{ GeneIdFilters, GeneSymbolFilters, PrimaryIdsValidator, TranscriptIdFilters }
import play.api.Configuration
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.{ Controller, RequestHeader, Result }


// ===========================================================================
class GenomicData @javax.inject.Inject() (
      configuration: Configuration,
      context:       Context)
    extends Controller {

  
  private val repo = context.getService.service
  
  // ===========================================================================
  // by gene ID
  
  def getByGeneIds(
      gene_ids:   String,
      projection: Option[String]) =

    LoggingAction {
      implicit request =>          
        apply(
            primaryObject = GeneIdFilters,
            primaryIds    = splitCsv(gene_ids),
            projection    = projection)  
      }

  // ---------------------------------------------------------------------------
  def getByGeneIdsAndStudies(
      gene_ids:   String,
      study_ids:  String,
      projection: Option[String]) =

    LoggingAction {
      implicit request =>
        apply(
              primaryObject    = GeneIdFilters,
              primaryIds       = splitCsv(gene_ids),
              secondaryIds     = Some(Left(splitCsv(study_ids))),
              projection       = projection)
      }

  // ---------------------------------------------------------------------------
  def getByGeneIdsAndNormalizations(
      gene_ids:       String,
      normalizations: String,
      projection:     Option[String]) =

    LoggingAction {
      implicit request =>        
        apply(
              primaryObject  = GeneIdFilters,
              primaryIds     = splitCsv(gene_ids),
              normalizations = Some(normalizations),
              projection     = projection)
      }

  
  // ---------------------------------------------------------------------------
  def getByGeneIdsAndStudiesAndNormalizations(
      gene_ids:       String,
      study_ids:      String,
      normalizations: String,
      projection:     Option[String]) =
        
    LoggingAction {    
      implicit request =>      
        apply(
              primaryObject   = GeneIdFilters,
              primaryIds      = splitCsv(gene_ids),
              secondaryIds    = Some(Left(splitCsv(study_ids))),
              normalizations  = Some(normalizations),
              projection      = projection)  
      }
  
  // ===========================================================================
  // by gene symbol

  def getByGeneSymbols(
      gene_symbols: String,
      projection:   Option[String]) =
        
    LoggingAction {
      implicit request =>      
        apply(
              primaryObject = GeneSymbolFilters,
              primaryIds    = splitCsv(gene_symbols),
              projection    = projection)  
      }

  
  // ---------------------------------------------------------------------------
  def getByGeneSymbolsAndStudies(
      gene_symbols: String,
      study_ids:    String,
      projection:   Option[String]) =
        
    LoggingAction {    
      implicit request =>
        apply(
              primaryObject   = GeneSymbolFilters,
              primaryIds      = splitCsv(gene_symbols),
              secondaryIds    = Some(Left(splitCsv(study_ids))),
              projection      = projection)
      }

  
  // ---------------------------------------------------------------------------
  def getByGeneSymbolsAndNormalizations(
      gene_symbols:   String,
      normalizations: String,
      projection:     Option[String]) =
        
    LoggingAction {    
      implicit request =>      
        apply(
              primaryObject  = GeneSymbolFilters,
              primaryIds     = splitCsv(gene_symbols),
              normalizations = Some(normalizations),
              projection     = projection)
      }

  
  // ---------------------------------------------------------------------------
  def getByGeneSymbolsAndStudiesAndNormalizations(
      gene_symbols:   String,
      study_ids:      String,
      normalizations: String,
      projection:     Option[String]) =

    LoggingAction {
      implicit request =>
        apply(
              primaryObject   = GeneSymbolFilters,
              primaryIds      = splitCsv(gene_symbols),
              secondaryIds    = Some(Left(splitCsv(study_ids))),
              normalizations  = Some(normalizations),
              projection      = projection)
    }
  

  // ===========================================================================
  // by transcript ID
  
  def getByTranscriptIds(
      transcript_ids: String,
      projection:     Option[String]) =
      
    LoggingAction {    
      implicit request =>      
        apply(
              primaryObject = TranscriptIdFilters,
              primaryIds    = splitCsv(transcript_ids),
              projection    = projection)
      }

  
  // ---------------------------------------------------------------------------
  def getByTranscriptIdsAndStudies(
      transcript_ids: String,
      study_ids:      String,
      projection:     Option[String]) =
        
    LoggingAction {
    implicit request =>
      apply(
            primaryObject   = TranscriptIdFilters,
            primaryIds      = splitCsv(transcript_ids),
            secondaryIds    = Some(Left(splitCsv(study_ids))),
            projection      = projection)
  }

  
  // ---------------------------------------------------------------------------
  def getByTranscriptIdsAndNormalizations(
      transcript_ids: String,
      normalizations: String,
      projection:     Option[String]) = 
        
    LoggingAction {
      implicit request =>
        apply(
              primaryObject  = TranscriptIdFilters,
              primaryIds     = splitCsv(transcript_ids),
              normalizations = Some(normalizations),
              projection     = projection)
      }

  
  // ---------------------------------------------------------------------------
  def getByTranscriptIdsAndStudiesAndNormalizations(
      transcript_ids: String,
      study_ids:      String,
      normalizations: String,
      projection:     Option[String]) =

    LoggingAction {
      implicit request =>
        apply(
              primaryObject   = TranscriptIdFilters,
              primaryIds      = splitCsv(transcript_ids),
              secondaryIds    = Some(Left(splitCsv(study_ids))),
              normalizations  = Some(normalizations),
              projection      = projection)  
      }

  
  // ===========================================================================

  def getByGeneSymbolsAndTagsAndNormalizations(
      gene_symbols:   String,
      normalizations: String,
      projection:     Option[String]) =
      
    LoggingAction(bodyParser = parse.json) {
    
      implicit request =>
  
        val json: JsValue =
          request.body
  
          apply(
              primaryObject   = GeneSymbolFilters,
              primaryIds      = splitCsv(gene_symbols),
              secondaryIds    = Some(Right(json)),
              normalizations  = Some(normalizations),
              projection      = projection)
              
      }

  
  // ---------------------------------------------------------------------------
  def getByGeneIdsAndTagsAndNormalizations(
      gene_ids:       String,
      normalizations: String,
      projection:     Option[String]) =
        
    LoggingAction(bodyParser = parse.json) {
      implicit request =>
  
        val json = request.body
          apply(
              primaryObject   = GeneIdFilters,
              primaryIds      = splitCsv(gene_ids),
              secondaryIds    = Some(Right(json)),
              normalizations  = Some(normalizations),
              projection      = projection)
      }

  
  // ---------------------------------------------------------------------------
   def getByTranscriptIdsAndTagsAndNormalizations(
      transcript_ids: String,
      normalizations: String,
      projection:     Option[String]) =

    LoggingAction(bodyParser = parse.json) {
      implicit request =>
  
        val json = request.body
          apply(
              primaryObject   = TranscriptIdFilters,
              primaryIds      = splitCsv(transcript_ids),
              secondaryIds    = Some(Right(json)),
              normalizations  = Some(normalizations),
              projection      = projection)
      }

  
  // ---------------------------------------------------------------------------
  // by sample ID
  def getByTagsAndNormalizations(
      normalizations: String,
      projection:     Option[String]) =

    LoggingAction(bodyParser = parse.json) {
      implicit request =>
  
        val json: JsValue =
          request.body
  
          apply(
              secondaryIds    = Some(Right(json)),
              normalizations  = Some(normalizations),
              projection      = projection)
      }
          
  // ===========================================================================  
   private def apply(
                    primaryObject:   PrimaryIdsValidator                  = GeneIdFilters,
                    primaryIds:      Seq[String]                          = Nil,
                    secondaryIds:    Option[Either[Seq[String], JsValue]] = None,
                    normalizations:  Option[String]                       = None,
                    projection:      Option[String]                       = None)
          (implicit request:         RequestHeader): Result = {

     try {
         val input: Either[Seq[Error], InputFilters] = InputFilters(
                                                                    primaryObject,
                                                                    primaryIds,
                                                                    secondaryIds,
                                                                    normalizations,
                                                                    projection)
    
          input match {
           
            case Left(errorObject) =>{
              val error_str = errorObject
                                .map { error => s"""invalid ${error.key} -> ${error.value.mkString(",")}""" }
                                .mkString("\n")
               BadRequest(error_str)
            }
             
    
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
      
                   Ok(TsvFormatter.rnaSeq(
                                      genes,
                                      filters.normalization_combo.keySet.toSeq,
                                      _projection.get))
                                      
                }
              }  
           }
         }
     } catch {
         case x: IllegalStateException => BadRequest(x.getMessage)
         case x: Throwable             => BadRequest("Error while processing request")
     } 
  }

  
  // ---------------------------------------------------------------------------
  def splitCsv(csvIds: String): Seq[String] =
    csvIds
      .split(",", -1)
  
  // ===========================================================================
  // utils
  
}

// ===========================================================================
