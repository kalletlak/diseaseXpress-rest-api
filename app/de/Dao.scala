package de

import scala.collection.JavaConversions.iterableAsScalaIterable
import scala.sys.process.stringSeqToProcess

import com.fasterxml.jackson.databind.{ JsonNode, ObjectMapper }
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import play.api.libs.json.{ JsObject, Json }
//import play.api.libs.json.Json.toJsFieldJsValueWrapper
import de.model._
package object model {
  type Key = String
  type Value = Seq[String]
  type Field = String
}

class Dao(ctx: Context) {
  def get(filters: Map[Key, Value], fields: Seq[Field]): String = {
    val filterObj = filters.map {
      case (id, value) =>
        Json.obj(
          "terms" -> Json.obj(
            id -> value))
    }.toSeq
    val queryObj: JsObject =
      Json.obj(
        "query" -> Json.obj(
          "bool" -> Json.obj(
            "must" -> filterObj)),
        "_source" -> fields)
    val deploy = Seq("curl", "-X", "POST", ctx.url concat "/_search?size=" concat ctx.size.toString,
      "-H", "Content-Type: application/json", "-d", queryObj.toString())
    println(deploy.mkString(" "))
    (deploy!!)
  }

}

class Repository(ctx: Context) {

  val dao: Dao = new Dao(ctx)
  val mapper: ObjectMapper = new ObjectMapper();
  mapper.registerModule(DefaultScalaModule)

  def getGeneInfo() = {
    val response = dao.get(
      Map[Key, Value](),
      Seq("gene_info.symbol", "gene_info.transcripts.transcript_id"))
    mapper.readTree(response).get("hits").get("hits")

  }

  def getData(
    filters: Map[Key, Value],
    fields: Seq[Field]) = {
    val response = dao.get(filters, fields)
    val actualObj: JsonNode = mapper.readTree(response);
    val jsonNode1 = actualObj.get("hits").get("hits");
    jsonNode1.map { x =>
      {
        Json.obj("gene_id" -> x.get("_id"), "data" -> x.get("_source"))
      }
    }
      .toSeq

  }

  def getDataByFilteredSamples(
    filters: Map[Key, Value],
    sampleIds: Seq[String],
    fields: Seq[Field]) = {

    val response = dao.get(filters, fields)
    val actualObj: JsonNode = mapper.readTree(response);
    val jsonNode1 = actualObj.get("hits").get("hits");
    jsonNode1.map { geneObj =>
      {
        Json.obj(
          "gene_id" -> geneObj.get("_id"),
          "data" -> geneObj.get("_source")
            .get("samples")
            .filter { sampleObj => sampleIds.indexOf(sampleObj.get("sample_id").textValue()) >= 0 })
      }
    }
      .toSeq

  }
  def getDataByFilteredTranscripts(
    filters: Map[Key, Value],
    sampleIds: Seq[String],
    transcriptIds: Seq[String],
    fields: Seq[Field]) = {

    val response = dao.get(filters, fields)
    val actualObj: JsonNode = mapper.readTree(response);
    val jsonNode1 = actualObj.get("hits").get("hits");
    jsonNode1.map { geneObj =>
      {
        Json.obj(
          "gene_id" -> geneObj.get("_id"),
          "data" -> geneObj.get("_source")
            .get("samples")
            .filter { sampleObj => sampleIds.indexOf(sampleObj.get("sample_id").textValue()) >= 0 }
            .map { sampleObj =>
              Json.obj(
                "sample_id" -> sampleObj.get("sample_id"),
                "rsem" -> sampleObj.get("rsem"),
                "transcripts" -> sampleObj
                  .get("transcripts")
                  .filter { x =>
                    transcriptIds
                      .indexOf(x.get("transcript_id").textValue()) >= 0
                  })
            })
      }
    }
      .toSeq

  }

}

