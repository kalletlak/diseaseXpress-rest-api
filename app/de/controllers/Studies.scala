package de.controllers

import play.api.Configuration
import play.api.mvc.Controller
import play.api.libs.json.Json
import de.utils.{LoggingAction}
import de.repository.SamplesRepository

// ===========================================================================
class Studies @javax.inject.Inject()(
    conf: Configuration)
  extends Controller {
  def getStudies() =
    LoggingAction {
      implicit request =>
        Ok(Json.toJson(SamplesRepository.getStudies))
    }
}

// ===========================================================================
