package de.dao

import de.model.tags.Enums.study
import de.model.tags.Sample
import scala.io.Source
import de.utils.SampleUtils
import com.github.fakemongo.Fongo
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import com.mongodb.DBObject
import com.mongodb.util.JSON
import de.utils.Query
import com.fasterxml.jackson.databind.node.ObjectNode
import scala.collection.JavaConversions.asScalaIterator
import play.api.libs.json.JsValue.jsValueToJsLookup
import de.model.DomainTypes.{SampleId, StudyId}

object SamplesDAO {

  private val fongo = new Fongo("mongo_server");
  private val db = fongo.getDB("disease_express");
  private val jongo = new org.jongo.Jongo(db)
  private val collection = jongo.getCollection("samples")

  private val studySampleMap: Map[study, Seq[Sample]] = {

    //TODO: get url from configuration file
    val stream =
      Source
        .fromURL("https://s3.amazonaws.com/d3b.dam/disease-express/static-files/clinical_info.txt") // TODO: close

    stream
      .getLines
      .drop(1)
      .map(SampleUtils.apply)
      .toList
      .groupBy(_.study)
  }

  studySampleMap
    .values
    .flatten
    .toSeq
    .foreach { obj =>
      val json_obj: JsObject =
        Json.obj(
          "sample_id" ->
            obj.sample_id.formatJson,

          "tags" ->
            Json.toJson(obj.getAllTags))

      collection
        .insert(
          JSON
            .parse(json_obj.toString())
            .asInstanceOf[DBObject])
    }

  def getSamplesInfo(studies: Seq[StudyId] = Nil): Seq[Sample] =
    studies match {

      case Nil =>
        studySampleMap
          .values
          .flatten
          .toSeq

      case _ =>
        studies
          .flatMap(study.withNameOption)
          .flatMap(study_id =>
            studySampleMap
              .getOrElse(study_id, Seq())) // get samples for each study and flatten it all up
          .toSeq

    }

  def getStudies: Seq[String] =
    studySampleMap
      .keys
      .toSeq
      .map(_.entryName)

  def getSamples(query: Query): Seq[SampleId] = {
    val querystr = query.formatQuery
    val projection_str = s"""{sample_id: 1, _id: 0}"""

    collection
      .find(querystr)
      .projection(projection_str)
      .as(classOf[ObjectNode])
      .iterator()
      .map { obj =>
        Json
          .parse(obj.toString)
          .as[JsObject]
      }
      .map { obj =>
        (obj \ "sample_id").as[String]
      }
      .toSeq
  }

}