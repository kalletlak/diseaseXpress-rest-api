package de.controllers

import io.swagger.annotations.Api
import play.api.mvc.Controller
import io.swagger.annotations.ApiOperation
import play.api.libs.json.Json
import de.utils.SampleDataUtil
import de.utils.LoggingAction

@Api(value = "/Studies",
      description = "Operations with Todos")
class Studies @javax.inject.Inject()(configuration: play.api.Configuration)
  extends Controller {
  @ApiOperation(value = "get All Studies",
                 notes = "Returns List of Studies",
                 response = classOf[String],
                 responseContainer = "List",
                 produces = "application/json",
                 httpMethod = "GET")
  def getStudies() = LoggingAction {
    implicit request =>
      Ok(Json.toJson(SampleDataUtil.getStudies))
  }
}
