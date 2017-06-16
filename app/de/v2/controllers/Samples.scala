package de.v2.controllers

import io.swagger.annotations.Api
import play.api.mvc.Controller
import io.swagger.annotations.ApiOperation
import play.api.mvc.Action
import play.api.libs.json.Json
import de.v2.utils.SampleDataUtil
import play.api.mvc.Accepting
import play.api.mvc.RequestHeader
import de.v2.utils.SampleAnnotation
import de.v2.model.DomainTypes.StudyId

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
      case None    => SampleDataUtil.getStudies()
    }

    val response = SampleDataUtil.getSamplesInfo(_studies)
    render {
      case Accepts.Json() => Ok(Json.toJson(response))
      case AcceptsTsv() => {
        val header = Seq(SampleAnnotation.header.mkString("\t"))
        val data = response.map { _.values.mkString("\t") }
        Ok((header ++ data).mkString("\n"))
      }
    }

  }
  @ApiOperation(value = "get All Samples data",
    notes = "Returns List of Sample Data",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getAllSamples() = Action {
    implicit request =>
      getData(None)
  }

  @ApiOperation(value = "get Samples Data",
    notes = "Returns List of Sample Data",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getSamples(studyIds: String) = Action {
    implicit request =>
      getData(Some(studyIds))
  }

}
