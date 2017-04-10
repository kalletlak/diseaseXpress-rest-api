package de

import scala.collection.JavaConversions.iterableAsScalaIterable
import scala.sys.process.stringSeqToProcess

import com.fasterxml.jackson.databind.{ JsonNode, ObjectMapper }
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import play.api.libs.json.{ JsObject, Json }
//import play.api.libs.json.Json.toJsFieldJsValueWrapper

class Base {
  type Key = String
  type Value = Seq[String]
  type Field = String
}
class Dao extends Base {
  def get(filters: Map[Key, Value], fields: Seq[Field])(implicit ctx: Context): String = {
    println("came to get dao")
    val filterObj = filters.map {
      case (id, value) =>
        {
          val filter: JsObject =
            Json.obj(
              id -> value)
          Json.obj(
            "terms" -> filter)
        }
    }.toSeq
    val queryObj: JsObject =
      Json.obj(
        "query" -> Json.obj(
          "bool" -> Json.obj(
            "must" -> Json.toJson(filterObj))),
        "_source" -> Json.toJson(fields))
    val deploy = Seq("curl", "-X", "POST", ctx.url concat "/_search?size=" concat ctx.size.toString,
      "-H", "Content-Type: application/json", "-d", queryObj.toString())
    println(deploy.mkString(" "))
    (deploy.!!)
  }

}

class Repository extends Base {

  val dao: Dao = new Dao {}
  val mapper: ObjectMapper = new ObjectMapper();
  mapper.registerModule(DefaultScalaModule)

  def getGeneInfo()(implicit ctx: Context) = {
    val response = dao.get(
      Map[String, Seq[String]](),
      Seq("gene_info.symbol", "gene_info.transcripts.transcript_id"))
    mapper.readTree(response).get("hits").get("hits")

  }

  def getData(filters: Map[Key, Value], fields: Seq[Field])(implicit ctx: Context) = {
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

  def getDataByFilteredSamples(filters: Map[Key, Value], sampleIds: Seq[String], fields: Seq[Field])(implicit ctx: Context) = {
    val response = dao.get(filters, fields)
    val actualObj: JsonNode = mapper.readTree(response);
    val jsonNode1 = actualObj.get("hits").get("hits");
    jsonNode1.map { x =>
      {
        Json.obj(
          "gene_id" -> x.get("_id"),
          "data" -> x.get("_source")
            .get("samples")
            .filter { x => sampleIds.indexOf(x.get("sample_id").textValue()) >= 0 })
      }
    }
      .toSeq

  }
  def getDataByFilteredTranscripts(filters: Map[Key, Value], sampleIds: Seq[String], transcriptIds: Seq[String], fields: Seq[Field])(implicit ctx: Context) = {
    val response = dao.get(filters, fields)
    val actualObj: JsonNode = mapper.readTree(response);
    val jsonNode1 = actualObj.get("hits").get("hits");
    jsonNode1.map { x =>
      {
        Json.obj(
          "gene_id" -> x.get("_id"),
          "data" -> x.get("_source")
            .get("samples")
            .filter { x => sampleIds.indexOf(x.get("sample_id").textValue()) >= 0 }
            .map { x =>
              Json.obj(
                "sample_id" -> x.get("sample_id"),
                "rsem" -> x.get("rsem"),
                "transcripts" -> x
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

