package de.controllers

import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.json.{JsValue, JsString}
import play.api.mvc.{Accepting, Controller, RequestHeader, Result}
import de.utils.{LoggingAction}
import de.model.DomainTypes.StudyId
import de.repository.SamplesRepository
import de.validators.StudyIdFilters

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
    
        StudyIdFilters(studyIds) match {
          
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
