package de.controllers

import play.api.mvc.Controller
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.mvc.Accepting
import de.model.output.GeneInfo
import de.model.output.TranscriptWithGeneInfo
import de.utils.LoggingAction
import de.repository.GeneRepository

// ===========================================================================
class Transcripts @javax.inject.Inject() (
      configuration: play.api.Configuration)
    extends Controller {

  // ---------------------------------------------------------------------------
  def getTranscriptIds() =
    LoggingAction {
      Ok(Json.toJson(GeneRepository.getTranscriptIds))
    }
  
  // ---------------------------------------------------------------------------
  def getTranscriptInfo(transcript_ids: String) =
    LoggingAction {
      implicit request =>
        
        val transcripts: Seq[TranscriptWithGeneInfo] =
          transcript_ids
            .split(",", -1).toSeq
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
