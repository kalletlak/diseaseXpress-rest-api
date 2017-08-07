package de.v2.controllers

import io.swagger.annotations.Api
import play.api.mvc.Controller
import io.swagger.annotations.ApiOperation
import play.api.libs.json.Json
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import io.swagger.annotations.ApiResponses
import io.swagger.annotations.ApiResponse
import play.api.mvc.Accepting
import de.v2.utils.GeneDataUtil
import de.v2.model.GeneInfoOutput
import de.v2.model.TranscriptWithGeneInfoOutput
import de.v2.utils.LoggingAction

@Api(value = "/Genes", description = "Operations with Genes and Transcripts")
class Genes @javax.inject.Inject()(
                                    configuration: play.api.Configuration)
  extends Controller {
  val AcceptsTsv = Accepting("text/tab-separated-values")

  /**
    * Converts gene data object to tsv format
    */
  private def tsvFormat(result: Seq[GeneInfoOutput]) = {
    val header = Seq(GeneInfoOutput.getHeader.mkString("\t"))
    val t1 = result
      .flatMap { GeneInfoOutput.getValues }
      .map { _.mkString("\t") }
    val data = result.flatMap { x =>
      GeneInfoOutput
        .getValues(x)
        .map { _.mkString("\t") }
    }
    (header ++ t1).mkString("\n")
  }

  @ApiOperation(value = "get All Genes",
                 notes = "Returns List of Gene Ids",
                 responseContainer = "List",
                 response = classOf[String],
                 produces = "application/json",
                 httpMethod = "GET")
  def getGeneIds() = LoggingAction {
    Ok(Json.toJson(GeneDataUtil.getGeneIds()))
  }

  @ApiOperation(value = "get All Genes",
                 notes = "Returns List of Gene Symbols",
                 response = classOf[String],
                 responseContainer = "List",
                 produces = "application/json",
                 httpMethod = "GET")
  def getGenesBySymbol() = LoggingAction {
    Ok(Json.toJson(GeneDataUtil.getGeneSymbols()))
  }

  @ApiOperation(value = "get All Transcripts",
                 notes = "Returns List of Transcript Ids",
                 response = classOf[String],
                 responseContainer = "List",
                 produces = "application/json",
                 httpMethod = "GET")
  def getTranscriptIds() = LoggingAction {
    Ok(Json.toJson(GeneDataUtil.getTranscriptIds()))
  }

  @ApiOperation(value = "get Gene information",
                 notes = "Returns Gene information",
                 response = classOf[GeneInfoOutput],
                 responseContainer = "List",
                 produces = "application/json, text/tab-separated-values",
                 httpMethod = "GET")
  def getGeneInfoByIds(gene_ids: String) = LoggingAction {
    implicit request =>
      val result = gene_ids.split(",")
        .map { GeneDataUtil.getGeneById }
        .filter { _.isDefined }
        .map { _.get }
      render {
        case Accepts.Json() => Ok(Json.toJson(result))
        case AcceptsTsv()   => Ok(tsvFormat(result))
      }

  }

  @ApiOperation(value = "get Gene information",
                 notes = "Returns Gene information",
                 response = classOf[GeneInfoOutput],
                 responseContainer = "List",
                 produces = "application/json, text/tab-separated-values",
                 httpMethod = "GET")
  def getGeneInfoBySymbols(gene_symbols: String) = LoggingAction {
    implicit request =>
      val result = gene_symbols.split(",")
        .map { GeneDataUtil.getGeneBySymbol }
        .filter { _.isDefined }
        .map { _.get }
      render {
        case Accepts.Json() => Ok(Json.toJson(result))
        case AcceptsTsv()   => Ok(tsvFormat(result))
      }
  }

  @ApiOperation(value = "get Transcript information",
                 notes = "Returns Transcript information",
                 response = classOf[TranscriptWithGeneInfoOutput],
                 responseContainer = "List",
                 produces = "application/json, text/tab-separated-values",
                 httpMethod = "GET")
  def getTranscriptInfo(transcript_ids: String) = LoggingAction {
    implicit request =>
      val result = transcript_ids.split(",")
        .map { GeneDataUtil.getTranscript }
        .filter { _.isDefined }
        .map { _.get }
        .flatMap { obj => {
          obj.transcripts
            .map { transcript =>
              TranscriptWithGeneInfoOutput.apply(transcript, obj.gene_id,
                                                  obj.gene_symbol)
            }
        }
        }

      render {
        case Accepts.Json() => Ok(Json.toJson(result))
        case AcceptsTsv()   => {

          val header = Seq(TranscriptWithGeneInfoOutput
            .getHeader
            .mkString("\t"))

          val data = result
            .map { TranscriptWithGeneInfoOutput.getValues }
            .map { _.mkString("\t") }

          Ok((header ++ data).mkString("\n"))
        }
      }

  }

}
