package de.controllers

import io.swagger.annotations.Api
import play.api.mvc.Controller
import io.swagger.annotations.ApiOperation
import play.api.libs.json.Json
import de.utils.SampleDataUtil
import play.api.mvc.Accepting
import play.api.mvc.RequestHeader
import de.model.DomainTypes.StudyId
import de.utils.LoggingAction

@Api(value = "/Samples",
  description = "Operations with Samples",
  produces = "application/json, text/tab-separated-values")
class Samples @javax.inject.Inject() (
  configuration: play.api.Configuration)
    extends Controller {

  val AcceptsTsv = Accepting("text/tab-separated-values")

  def getData(studies_ids: Option[StudyId])(implicit request: RequestHeader) = {
    val _studies = studies_ids match {
      case Some(x) => x.split(",").toSeq
      case None    => SampleDataUtil.getStudies
    }

    val response = SampleDataUtil.getSamplesInfo(_studies).map { sample => sample.getAllTagsAsMap }
    render {
      case Accepts.Json() => Ok(Json.toJson(response))
      case AcceptsTsv() => {
        val header = response.flatMap { sample_tags => sample_tags.keySet }.distinct
        val data = response.map { sample_tags =>
          {
            header.map { tag => sample_tags.getOrElse(tag, "") }.mkString("\t")

          }
        }
        Ok((Seq(header.mkString("\t")) ++ data).mkString("\n"))
      }
    }

  }

  @ApiOperation(value = "get All Samples data",
    notes = "Returns List of Sample Data",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getAllSamples() = LoggingAction {
    implicit request =>
      getData(None)
  }

  @ApiOperation(value = "get Samples Data",
    notes = "Returns List of Sample Data",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getSamples(studyIds: String) = LoggingAction {
    implicit request =>
      getData(Some(studyIds))
  }

}
