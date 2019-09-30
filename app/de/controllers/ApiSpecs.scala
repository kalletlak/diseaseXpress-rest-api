package de.controllers

import play.api.libs.concurrent.Execution.Implicits._
import com.iheart.playSwagger.SwaggerSpecGenerator
import javax.inject.Inject
import play.api.mvc.Controller
import play.api.cache.Cached
import play.api.mvc.Action
import scala.concurrent.Future

//https://github.com/iheartradio/play-swagger/blob/master/docs/AlternativeSetup.md
class ApiSpecs @Inject() extends Controller {
  implicit val cl = getClass.getClassLoader

  val domainPackage = "de.model.output"
  private lazy val generator = SwaggerSpecGenerator(domainPackage)
  
  def specs = Action.async { _ =>
    Future.fromTry(generator.generate()).map(Ok(_)) //generate() can also taking in an optional arg of the route file name. 
  }

}