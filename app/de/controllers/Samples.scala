package de.controllers

import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.json.{JsValue, JsString}
import play.api.mvc.{Accepting, Controller, RequestHeader, Result}
import io.swagger.annotations.{Api, ApiOperation}
import de.utils.{SampleDataUtil, LoggingAction}
import de.model.DomainTypes.StudyId

// ===========================================================================
@Api(
  value       = "/Samples",
  description = "Operations with Samples",
  produces    = "application/json, text/tab-separated-values")
class Samples @javax.inject.Inject() (
      configuration: Configuration)
    extends Controller {

  // ---------------------------------------------------------------------------
  @ApiOperation(
		httpMethod        = "GET",
		response          = classOf[String],
		responseContainer = "List",
    value             = "Get all samples data",
    notes             = "Returns list of sample data")
  def getAllSamples() =
    LoggingAction {
      implicit request =>
        apply(studyIdsOpt = None)
    }

  // ---------------------------------------------------------------------------
  @ApiOperation(
		httpMethod        = "GET",
		response          = classOf[String],
		responseContainer = "List",
    value             = "Get samples data by study ID",
    notes             = "Returns list of sample data filtered by study ID")
  def getSamplesByStudy(studyIds: String) =
    LoggingAction {
      implicit request =>
        apply(studyIdsOpt = Some(studyIds))
    }

  // ===========================================================================  
  private def apply
        (         studyIdsOpt: Option[StudyId])
        (implicit request:     RequestHeader)
      : Result = {
    
    // TODO: input validation
    
    val studyIds: Seq[String] =
      studyIdsOpt match {
        case Some(x) => x.split(",", -1).toSeq
        case None    => SampleDataUtil.getStudies
      }

    val response: Seq[Map[String, JsValue]] =
      SampleDataUtil
        .getSamplesInfo(studyIds)
        .map(_.getAllTagsAsMap)

    render {
      
      case Accepts.Json() =>
        Ok(Json.toJson(response))
        
      case Play.AcceptsTsv() =>
        Ok(TsvFormatter(response))

    }

  }

}

// ===========================================================================
