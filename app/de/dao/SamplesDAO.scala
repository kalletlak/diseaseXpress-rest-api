package de.dao

import scala.collection.JavaConversions.asScalaIterator
import scala.io.Source

import com.fasterxml.jackson.databind.node.ObjectNode
import com.github.fakemongo.Fongo
import com.mongodb.DBObject
import com.mongodb.util.JSON

import de.model.DomainTypes.{ SampleId, StudyId }
import de.model.tags.Sample
import de.utils.{ Query, SampleUtils }
import io.swagger.annotations.ApiModel
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper

object SamplesDAO {

  private val fongo      = new Fongo("mongo_server");
  private val db         = fongo.getDB("disease_express");
  private val jongo      = new org.jongo.Jongo(db)
  private val collection = jongo.getCollection("samples")

  private val studySampleMap: Map[StudyId, Seq[Sample]] = {

    //TODO: get url from configuration file
    val stream =
      Source
        .fromURL("https://s3.amazonaws.com/d3b.dam/disease-express/static-files/clinical_info.txt")

    val results = stream
      .getLines
      .drop(1)
      .map(SampleUtils.apply)
      .toList
      .groupBy(_.study.value)

    stream.close()
    results
  }
  
  // ---------------------------------------------------------------------------
  //insert samples into in-memory mongodb
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
  // ---------------------------------------------------------------------------
  def getSamplesInfo(studies: Seq[StudyId] = getStudies): Seq[Sample] =
    studies
      .flatMap { studySampleMap.get }
      .flatten
      
  // ---------------------------------------------------------------------------
  def getStudies: Seq[String] =
    studySampleMap
      .keys
      .toSeq
  
  // ---------------------------------------------------------------------------
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

  // ---------------------------------------------------------------------------
  def isStudyPresent(studyId: String) = studySampleMap.get(studyId).isDefined

}