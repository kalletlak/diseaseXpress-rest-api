package de.utils

import scala.collection.JavaConversions.asScalaIterator
import scala.io.Source

import com.fasterxml.jackson.databind.node.ObjectNode
import com.github.fakemongo.Fongo
import com.mongodb.DBObject
import com.mongodb.util.JSON

import de.model.Domain.Sample
import de.model.Enums.study
import io.swagger.annotations.ApiModel
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json

// ===========================================================================
object SampleDataUtil {
  
  val studySampleMap: Map[study, Seq[Sample]] = {

    //TODO: get url from configuration file
    val stream =
      Source
        .fromURL("https://s3.amazonaws.com/d3b.dam/disease-express/static-files/clinical_info.txt") // TODO: close

    stream
      .getLines
      .drop(1)
      .map(Sample.apply)
      .toList
      .groupBy(_.study)
  }

  // ---------------------------------------------------------------------------
  def getStudies: Seq[String] =
    studySampleMap
      .keys
      .toSeq
      .map(_.entryName)

  // ---------------------------------------------------------------------------
  def getSamples(studies: Seq[String] = Nil): Seq[String] =
    studies
      match {
    
        case Nil =>
          studySampleMap
            .flatMap {
              case (_, sampleAnnotations) =>
                sampleAnnotations
                  .map(_.sample_id.value) }
            .toSeq

        case _ =>    
          studies
            .flatMap(study.withNameOption)
            .flatMap(studySampleMap) // get samples for each study and flatten it all up
            .map(_.sample_id)
            .map { _.value }        
            .toSeq
    
      }

  // ---------------------------------------------------------------------------
  def getSamplesInfo(studies: Seq[String] = Nil): Seq[Sample] =
    studies
      match {
    
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

  // ---------------------------------------------------------------------------
  private val fongo      = new Fongo("mongo_server");
  private val db         = fongo.getDB("disease_express");
  private val jongo      = new org.jongo.Jongo(db)
  private val collection = jongo.getCollection("samples")

  // FIXME
  getSamplesInfo()
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
            .asInstanceOf[DBObject]) }

  // ---------------------------------------------------------------------------
  def getSamples(value: JsValue): Seq[String] = 
    MongoQuery
      .getMongoQuery(value)
       match {
      
        case query: Query =>
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
                .as[JsObject] }
            .map { obj =>
              (obj \ "sample_id").as[String] }
            .toSeq
        
        case _ =>
          Seq()

      }

}

// ===========================================================================
