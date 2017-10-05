package de.controllers

import play.api.Configuration
import play.api.mvc.Controller
import play.api.libs.json.Json
import io.swagger.annotations.{Api, ApiOperation}
import de.utils.{SampleDataUtil, LoggingAction}

// ===========================================================================
@Api(
  value       = "/Studies",
  description = "Operations with Todos")
class Studies @javax.inject.Inject()(
    conf: Configuration)
  extends Controller {
  @ApiOperation(
	  httpMethod        = "GET",
	  response          = classOf[String],
	  produces          = "application/json",
    value             = "Get all studies",
    notes             = "Returns list of studies",
    responseContainer = "List")
  def getStudies() =
    LoggingAction {
      implicit request =>
        Ok(Json.toJson(SampleDataUtil.getStudies))
    }
}

// ===========================================================================
