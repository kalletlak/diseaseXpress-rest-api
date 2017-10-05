package de.controllers

import io.swagger.annotations.Api
import play.api.mvc.Controller
import io.swagger.annotations.ApiOperation
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import io.swagger.annotations.ApiResponses
import io.swagger.annotations.ApiResponse
import play.api.mvc.Accepting
import de.utils.GeneDataUtil
import de.model.GeneInfo
import de.model.TranscriptWithGeneInfo
import de.utils.LoggingAction
import de.model.Inputs.GeneSymbolQuery

@Api(value = "/Genes", description = "Operations with Genes and Transcripts")
class Genes @javax.inject.Inject() (
  configuration: play.api.Configuration)
    extends Controller {
  val AcceptsTsv = Accepting("text/tab-separated-values")

  /**
   * Converts gene data object to tsv format
   */
  private def tsvFormat(result: Seq[GeneInfo]) = {
    val header = Seq(GeneInfo.getHeader.mkString("\t"))
    val data = result
      .flatMap(GeneInfo.getValues)
      .map(_.mkString("\t"))

    (header ++ data).mkString("\n")
  }

  @ApiOperation(value = "get All Genes",
    notes = "Returns List of Gene Ids",
    responseContainer = "List",
    response = classOf[String],
    produces = "application/json",
    httpMethod = "GET")
  def getGeneIds() = LoggingAction {
    Ok(Json.toJson(GeneDataUtil.getGeneIds))
  }

  @ApiOperation(value = "get All Genes",
    notes = "Returns List of Gene Symbols",
    response = classOf[String],
    responseContainer = "List",
    produces = "application/json",
    httpMethod = "GET")
  def getGenesBySymbol() = LoggingAction {
    Ok(Json.toJson(GeneDataUtil.getGeneSymbols))
  }

  @ApiOperation(value = "get All Transcripts",
    notes = "Returns List of Transcript Ids",
    response = classOf[String],
    responseContainer = "List",
    produces = "application/json",
    httpMethod = "GET")
  def getTranscriptIds() = LoggingAction {
    Ok(Json.toJson(GeneDataUtil.getTranscriptIds))
  }

  @ApiOperation(value = "get Gene information",
    notes = "Returns Gene information",
    response = classOf[GeneInfo],
    responseContainer = "List",
    produces = "application/json, text/tab-separated-values",
    httpMethod = "GET")
  def getGeneInfoByIds(gene_ids: String) = LoggingAction {
    implicit request =>
      val result = gene_ids.split(",")
        .flatMap { GeneDataUtil.getGeneById }
      render {
        case Accepts.Json() => Ok(Json.toJson(result))
        case AcceptsTsv()   => Ok(tsvFormat(result))
      }

  }

  @ApiOperation(value = "get Gene information",
    notes = "Returns Gene information",
    response = classOf[GeneInfo],
    responseContainer = "List",
    produces = "application/json, text/tab-separated-values",
    httpMethod = "GET")
  def getGeneInfoBySymbols(gene_symbols: String) = LoggingAction {
    implicit request =>
      val geneInputRef = GeneSymbolQuery(gene_symbols.split(",", -1).map { _.trim })
      val result = GeneDataUtil.getGeneInputRef(geneInputRef)
      /*  val result = gene_symbols.split(",")
        .flatMap { GeneDataUtil.getGeneBySymbol }*/
      render {
        case Accepts.Json() => Ok(Json.toJson(result))
        case AcceptsTsv()   => Ok(tsvFormat(result))
      }
  }

  @ApiOperation(value = "get Transcript information",
    notes = "Returns Transcript information",
    response = classOf[TranscriptWithGeneInfo],
    responseContainer = "List",
    produces = "application/json, text/tab-separated-values",
    httpMethod = "GET")
  def getTranscriptInfo(transcript_ids: String) = LoggingAction {
    implicit request =>
      val result = transcript_ids.split(",")
        .flatMap { GeneDataUtil.getTranscript }
        .flatMap { obj =>
          {
            obj.transcripts
              .map { transcript =>
                TranscriptWithGeneInfo.apply(transcript, obj.gene_id,
                  obj.gene_symbol)
              }
          }
        }

      render {
        case Accepts.Json() => Ok(Json.toJson(result))
        case AcceptsTsv() => {

          val header = Seq(TranscriptWithGeneInfo
            .getHeader
            .mkString("\t"))

          val data = result
            .map { TranscriptWithGeneInfo.getValues }
            .map { _.mkString("\t") }

          Ok((header ++ data).mkString("\n"))
        }
      }

  }

}
