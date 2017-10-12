package de.controllers

import de.model.output.GeneInfo
import de.repository.GeneRepository
import de.utils.LoggingAction
import de.validators.{ GeneIdFilters, GeneSymbolQuery }
import play.api.libs.json.Json
import play.api.mvc.Controller
import utils.Implicits.AnythingImplicits
import de.validators.GeneSymbolFilters

// ===========================================================================
class Genes @javax.inject.Inject() (
      configuration: play.api.Configuration)
    extends Controller {
  
  // ---------------------------------------------------------------------------
  def getGeneIds() =
    LoggingAction {
      Ok(Json.toJson(GeneRepository.getGeneIds))
    }

  // ---------------------------------------------------------------------------
  def getGenesBySymbol() =
    LoggingAction {
      Ok(Json.toJson(GeneRepository.getGeneSymbols))
    }

  // ---------------------------------------------------------------------------
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
