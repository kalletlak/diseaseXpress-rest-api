package de.utils

import java.nio.charset.Charset

import scala.annotation.implicitNotFound
import scala.concurrent.Future

import dispatch.{ Http, as, implyRequestHandlerTuple, url }
import dispatch.Defaults.executor
import play.Play
import play.api.libs.json.Json
import play.api.mvc.{ ActionBuilder, Request, Result }
import play.api.libs.json.JsObject
import java.net.URLDecoder

object LoggingAction extends ActionBuilder[Request] {
  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) = {
    val route_pattern_regex = "[a-zA-Z0-9_\\/\\^\\[\\]]*[\\$]([a-zA-Z_]+)\\<\\[\\^\\/\\]\\+\\>".r

    val queryParams = request.queryString

    val requestPath = URLDecoder.decode(request.path, "UTF-8")

    val params = request.tags.get("ROUTE_PATTERN") match {
      case Some(route_pattern) => {
        val to_return =
          route_pattern_regex
            .findAllMatchIn(route_pattern)
            .map { matched_pattern =>
              val param_name = matched_pattern.group(1);

              val route_identifier_path =
                matched_pattern
                  .matched
                  .split("\\/\\$")(0)
              val request_pattern = s"""[a-zA-Z0-9%_/]*$route_identifier_path/([a-zA-Z0-9_,]*)[/]*""".replace("/", "\\/").r

              val param_value = request_pattern
                .findFirstMatchIn(requestPath) match {
                  case Some(matched_request) => Some(matched_request.group(1).split(",", -1).toSeq)
                  case _                     => Some(Seq())
                }
              (param_name, param_value.get)
            }
        Some(to_return.toMap)
      }
      case _ => None
    }

    val eventsUrl = Play
      .application
      .configuration
      .getString("disease-express.events-tracker.url")
    val username = Play
      .application
      .configuration
      .getString("disease-express.events-tracker.username")
    val password = Play
      .application
      .configuration
      .getString("disease-express.events-tracker.password")

    val myRequest = url(eventsUrl).as_!(username, password)

    val myRequestAsJson = myRequest.setContentType("application/json", Charset.defaultCharset)

    val completeParams = params.get ++ queryParams

    val jsonStr = Json.toJson(Json.obj("uri" -> request.uri, "params" -> completeParams))

    val myPostWithBody = myRequestAsJson << s"""$jsonStr"""

    //make async request
    val response: Future[String] = Http.default(myPostWithBody > as.String)

    block(request)
  }
}
