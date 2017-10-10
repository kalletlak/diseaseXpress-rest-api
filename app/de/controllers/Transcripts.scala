package de.controllers

import play.api.mvc.Controller
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.mvc.Accepting
import io.swagger.annotations.Api
import io.swagger.annotations.ApiOperation
import io.swagger.annotations.ApiResponses
import io.swagger.annotations.ApiResponse
import de.model.output.GeneInfo
import de.model.output.TranscriptWithGeneInfo
import de.utils.LoggingAction
import de.repository.GeneRepository

// ===========================================================================
@Api(
  value = "/Transcripts",
  description = "Operations with Transcripts")
class Transcripts @javax.inject.Inject() (
      configuration: play.api.Configuration)
    extends Controller {

  // ---------------------------------------------------------------------------
  @ApiOperation(
    notes             = "Returns List of Transcript Ids",
    response          = classOf[String],
    value             = "get All Transcripts",
    responseContainer = "List",
    produces          = "application/json",
    httpMethod        = "GET")
  def getTranscriptIds() =
    LoggingAction {
      Ok(Json.toJson(GeneRepository.getTranscriptIds))
    }
  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get Transcript information",
    notes             = "Returns Transcript information",
    response          = classOf[TranscriptWithGeneInfo],
    responseContainer = "List",
    produces          = "application/json, text/tab-separated-values",
    httpMethod        = "GET")
  def getTranscriptInfo(transcript_ids: String) =
    LoggingAction {
      implicit request =>
        
        val transcripts: Seq[TranscriptWithGeneInfo] =
          transcript_ids
            .split(",", 1).toSeq
            .flatMap(GeneRepository.getTranscriptId)
            .flatMap { gene =>              
                gene
                  .transcripts
                  .map(TranscriptWithGeneInfo(
                    gene.gene_id, gene.gene_symbol)) }
  
        render {
          
          case Accepts.Json() =>
            Ok(Json.toJson(transcripts))
            
          case Play.AcceptsTsv() =>            
            Ok(TsvFormatter.transcriptInfo(transcripts))

        }
  
    }

}

// ===========================================================================