package de.controllers

import de.model.DomainTypes.StudyId
import de.repository.SamplesRepository
import de.utils.LoggingAction
import de.validators.SecondaryIds
import play.api.Configuration
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.{ Controller, RequestHeader, Result }

// ===========================================================================
class Samples @javax.inject.Inject() (
      configuration: Configuration)
    extends Controller {

  // ---------------------------------------------------------------------------
  def getAllSamples() =
    LoggingAction {
      implicit request =>
        apply()
    }

  // ---------------------------------------------------------------------------
  def getSamplesByStudy(studyIds: String) =
    LoggingAction {
      implicit request =>
        apply(studyIds = studyIds.split(",", -1).toSeq)
    }

  // ===========================================================================  
  private def apply
        (         studyIds: Seq[StudyId]=Nil)
        (implicit request:     RequestHeader)
      : Result = {
    
        SecondaryIds(Left(studyIds)) match {
          
          case Left(errorObject) =>
            BadRequest(Json.toJson(errorObject.formatJson))

          case Right(studyQuery) => {
            
            val response: Seq[Map[String, JsValue]] =
              SamplesRepository
                .getSamplesInfo(studyQuery.ref_id)
                .map(_.getAllTagsAsMap)
                
            render {
              case Accepts.Json()    => Ok(Json.toJson(response))
              case Play.AcceptsTsv() => Ok(TsvFormatter(response))
            }
            
          }
        }
  }

}

// ===========================================================================
