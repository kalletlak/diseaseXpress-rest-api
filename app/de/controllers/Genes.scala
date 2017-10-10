package de.controllers

import de.model.output.GeneInfo
import de.repository.GeneRepository
import de.utils.LoggingAction
import de.validators.{ GeneIdFilters, GeneSymbolQuery }
import io.swagger.annotations.{ Api, ApiModel, ApiOperation }
import play.api.libs.json.Json
import play.api.mvc.Controller
import utils.Implicits.AnythingImplicits
import de.validators.GeneSymbolFilters

// ===========================================================================
@Api(
  value       = "/Genes",
  description = "Operations with Genes")
class Genes @javax.inject.Inject() (
      configuration: play.api.Configuration)
    extends Controller {
  
  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get All Genes",
    notes             = "Returns List of Gene Ids",
    responseContainer = "List",
    response          = classOf[String],
    produces          = "application/json",
    httpMethod        = "GET")
  def getGeneIds() =
    LoggingAction {
      Ok(Json.toJson(GeneRepository.getGeneIds))
    }

  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get All Genes",
    notes             = "Returns List of Gene Symbols",
    response          = classOf[String],
    responseContainer = "List",
    produces          = "application/json",
    httpMethod        = "GET")
  def getGenesBySymbol() =
    LoggingAction {
      Ok(Json.toJson(GeneRepository.getGeneSymbols))
    }

  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get Gene information",
    notes             = "Returns Gene information",
    response          = classOf[GeneInfo],
    responseContainer = "List",
    produces          = "application/json, text/tab-separated-values",
    httpMethod        = "GET")
  def getGeneInfoByIds(gene_ids: String) =
    LoggingAction {
      implicit request =>

        val result = GeneIdFilters(gene_ids.split(",", -1))

        result match {
          case Left(errorObject) =>
            BadRequest(Json.toJson(errorObject.formatJson))

          case Right(genes) => render {
            case Accepts.Json()    => Ok(Json.toJson(genes))
            case Play.AcceptsTsv() => Ok(TsvFormatter.geneInfo(genes))
          }
        }
    }

  // ---------------------------------------------------------------------------
  @ApiOperation(
    value             = "get Gene information",
    notes             = "Returns Gene information",
    response          = classOf[GeneInfo],
    responseContainer = "List",
    produces          = "application/json, text/tab-separated-values",
    httpMethod        = "GET")
  def getGeneInfoBySymbols(gene_symbols: String) =
    LoggingAction {
      implicit request =>

        val result = GeneSymbolFilters(gene_symbols.split(",", -1))

        result match {
          case Left(errorObject) =>
            BadRequest(Json.toJson(errorObject.formatJson))

          case Right(genes) => render {
            case Accepts.Json()    => Ok(Json.toJson(genes))
            case Play.AcceptsTsv() => Ok(TsvFormatter.geneInfo(genes))
          }
        }
  }

}

// ===========================================================================
