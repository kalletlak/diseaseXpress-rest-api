package de.controllers

import play.api.mvc.Controller
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.mvc.Accepting
import io.swagger.annotations.Api
import io.swagger.annotations.ApiOperation
import io.swagger.annotations.ApiResponses
import io.swagger.annotations.ApiResponse
import de.model.output.GeneInfo
import de.utils.GeneDataUtil
import de.utils.LoggingAction
import utils.Implicits.AnythingImplicits
import de.validators.GeneSymbolQuery

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
      Ok(Json.toJson(GeneDataUtil.getGeneIds))
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
      Ok(Json.toJson(GeneDataUtil.getGeneSymbols))
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
        
        val genes: Seq[GeneInfo] =
          gene_ids
            .split(",", -1)
            .flatMap(GeneDataUtil.getGeneById)

        render {
          case Accepts.Json() =>
            Ok(Json.toJson(genes))
            
          case Play.AcceptsTsv() =>
            Ok(TsvFormatter.geneInfo(genes))

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

        val genes: Seq[GeneInfo] = 
          GeneSymbolQuery(gene_symbols
            .split(",", -1)
            .toSeq)
          .zen {GeneDataUtil.getGeneInputRef}

        render {
          
          case Accepts.Json() =>
            Ok(Json.toJson(genes))
            
          case Play.AcceptsTsv() =>
            Ok(TsvFormatter.geneInfo(genes))
  
        }
  }

}

// ===========================================================================
