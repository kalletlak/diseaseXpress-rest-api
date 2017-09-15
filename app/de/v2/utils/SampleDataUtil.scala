package de.v2.utils

import java.io.InputStream

import scala.{ Left, Right }
import scala.collection.JavaConversions.asScalaIterator

import com.fasterxml.jackson.databind.node.ObjectNode
import com.github.fakemongo.Fongo

import de.v2.model.Enums.{ Tag, ethnicity, gender, group, library_type, platform, race, study, unavailable, vital_status }
import io.swagger.annotations.ApiModel
import play.api.libs.json.{ JsObject, JsValue }
import play.api.libs.json.{ Json, Writes }
import play.api.libs.json.JsValue.jsValueToJsLookup
import de.v2.model.Domain._
import com.mongodb.util.JSON
import com.mongodb.DBObject

object Sample {

  private val keyRegex = "[A-Za-z0-9_-]+".r
  private val valRegex = """[A-Za-z0-9_\-\/().>| *?#]+""".r

  private def readTags(tagString: String): Seq[Tag] = {
    if (!tagString.isEmpty()) {
      tagString
        .split(";", -1)
        .map { _.trim }
        .map {
          _.split("=").map { _.trim } match {
            case Array(k: String, v: String) =>
              require(keyRegex.pattern.matcher(k).matches())
              require(valRegex.pattern.matcher(v).matches())
              val value = try { Number(v) } catch { case _: Throwable => Text(v) }
              Tag(k, value)
          }
        }
        .toSeq
    } else {
      Seq()
    }

  }

  def readEnum[T](s: String, withNameOption: (String) => Option[T]): Either[T, unavailable] = {
    assert(!s.isEmpty())
    val temp: Either[T, unavailable] = withNameOption(s) match {
      case Some(x) => Left(x.asInstanceOf[T])
      case None    => Right(unavailable.withName(s))
    }
    temp
  }
  def readValue[Value](s: String, withNameOption: (String) => Value): Either[Value, unavailable] = {
    assert(!s.isEmpty())
    val temp: Either[Value, unavailable] = unavailable.withNameOption(s) match {
      case Some(x) => Right(x)
      case None => {
        Left(withNameOption(s))
      }
    }
    temp
  }

  def apply(line: String): Sample = {
    val spl = line.split("\t", -1).iterator
    val _sample_id = spl.next.trim
    val _patient_barcode = spl.next.trim
    val _sample_barcode = spl.next.trim
    val _group = spl.next.trim
    val _study = spl.next.trim
    val _disease = spl.next.trim
    val _disease_name = spl.next.trim
    val _disease_subtype = spl.next.trim
    val _tissue = spl.next.trim
    val _definition = spl.next.trim
    val _library_type = spl.next.trim
    val _platform = spl.next.trim
    val _center = spl.next.trim
    val _gender = spl.next.trim
    val _race = spl.next.trim
    val _ethnicity = spl.next.trim
    val _age_normal_tissue_collected_in_years = spl.next.trim
    val _age_at_diagnosis_in_days = spl.next.trim
    val _event_free_survival_time_in_days = spl.next.trim
    val _overall_survival_time_in_days = spl.next.trim
    val _vital_status = spl.next.trim
    val _tags = readTags(spl.next.trim)

    group.withName(_group) match {
      case group.normals => {
        NormalSample(Text(_sample_id),
          readValue(_patient_barcode, Text.apply),
          readValue(_sample_barcode, Text.apply),
          study.withName(_study),
          Text(_tissue),
          Text(_definition),
          readEnum(_library_type, library_type.withNameOption),
          readEnum(_platform, platform.withNameOption),
          Text(_center),
          readEnum(_gender, gender.withNameOption),
          readEnum(_race, race.withNameOption),
          readEnum(_ethnicity, ethnicity.withNameOption),
          readEnum(_vital_status, vital_status.withNameOption),
          _tags,
          _age_normal_tissue_collected_in_years.toFloat)
      }
      case group.tumors => {
        TumorSample(Text(_sample_id),
          readValue(_patient_barcode, Text.apply),
          readValue(_sample_barcode, Text.apply),
          study.withName(_study),
          Text(_tissue),
          Text(_definition),
          readEnum(_library_type, library_type.withNameOption),
          readEnum(_platform, platform.withNameOption),
          Text(_center),
          readEnum(_gender, gender.withNameOption),
          readEnum(_race, race.withNameOption),
          readEnum(_ethnicity, ethnicity.withNameOption),
          readEnum(_vital_status, vital_status.withNameOption),
          _tags,
          Text(_disease),
          Text(_disease_name),
          Text(_disease_subtype),
          readValue(_age_at_diagnosis_in_days, Number.apply),
          readValue(_event_free_survival_time_in_days, Number.apply),
          readValue(_overall_survival_time_in_days, Number.apply))
      }
      case _ => {
        assert(false, "Invalid group")
        ???
      }
    }
  }

  implicit val writeJson = new Writes[Sample] {
    def writes(obj: Sample): JsValue = {
      val tags = obj.tags.map { obj => (obj.key -> obj.value.formatJson) }.toMap
      /* Json.obj(
        "sample_id" -> obj.sample_id,
        "patient_barcode" -> obj.patient_barcode,
        "sample_barcode" -> obj.sample_barcode,
        "group" -> obj.group,
        "study" -> obj.study,
        "disease" -> obj.disease,
        "disease_name" -> obj.disease_name,
        "disease_subtype" -> obj.disease_subtype,
        "tissue" -> obj.tissue,
        "definition" -> obj.definition,
        "library_type" -> obj.library_type,
        "platform" -> obj.platform,
        "gender" -> obj.gender.entryName,
        "race" -> obj.race,
        "ethnicity" -> obj.ethnicity,
        "age_normal" -> obj.age_normal,
        "age_at_diagnosis_in_days" -> obj.age_at_diagnosis_in_days,
        "event_free_survival_time_in_days" -> obj.event_free_survival_time_in_days,
        "overall_survival_time_in_days" -> obj.overall_survival_time_in_days,
        "vital_status" -> obj.vital_status).deepMerge(Json.toJson(tags).asInstanceOf[JsObject])*/
      Json.toJson(obj.getAllTagsAsMap)
    }
  }

}

object SampleDataUtil {
  val studySampleMap: Map[study, Seq[Sample]] = {
    val stream: InputStream = getClass
      .getResourceAsStream("/clinical_info.txt")
    val src = scala.io.Source.fromInputStream(stream)
    val x = src.getLines
      .drop(1)
      .map {
        Sample.apply
      }
      .toList
      .groupBy {
        _.study
      }
    src.close()
    x
  }

  def getStudies: Seq[String] = studySampleMap.keys.toSeq.map { x => x.entryName }

  def getSamples(studies: Seq[String] = Nil): Seq[String] = studies match {
    case Nil => studySampleMap
      .flatMap {
        case (_, sampleAnnotations) => sampleAnnotations.map {
          _.sample_id
        }
      }
      .toSeq.map { x => x.value }
    case _ => {

      studies
        .flatMap { x => study.withNameOption(x) }
        .flatMap(studySampleMap) // get samples for each study and flatten it all up
        .map(_.sample_id)
        .toSeq.map { x => x.value }
    }

  }

  def getSamplesInfo(studies: Seq[String] = Nil): Seq[Sample] = studies match {
    case Nil => studySampleMap
      .values
      .flatten
      .toSeq
    case _ => studies
      .flatMap { x => study.withNameOption(x) }
      .flatMap(study_id => studySampleMap.getOrElse(study_id,
        Seq())) // get samples for each study and flatten it all up
      .toSeq
  }

  private val fongo = new Fongo("mongo_server");
  private val db = fongo.getDB("disease_express");

  private val jongo =
    new org.jongo.Jongo(db)

  private val collection = jongo.getCollection("samples")

  getSamplesInfo().foreach { obj =>
    {
      val json_obj = Json.obj(
        "sample_id" -> obj.sample_id.formatJson,
        "tags" -> Json.toJson(obj.getAllTags))

      collection.insert(JSON.parse(json_obj.toString()).asInstanceOf[DBObject])
    }
  }

  def getSamples(value: JsValue): Seq[String] = {
    MongoQuery.getMongoQuery(value) match {
      case query: Query => {
        val querystr = query.formatQuery
        val start = System.currentTimeMillis()
        val projection_str = s"""{sample_id: 1, _id: 0}"""
        val result = collection
          .find(querystr)
          .projection(projection_str)
          .as(classOf[ObjectNode])
          .iterator()
          .map { obj =>
            Json
              .parse(obj.toString)
              .as[JsObject]
          }
          .map { obj => (obj \ "sample_id").as[String] }
          .toSeq
        val executionTime = System.currentTimeMillis() - start
        println(s"""Execution Time to get samples \n\t query : $querystr \n\t Time : $executionTime""")
        result
      }
      case _ => Seq()
    }

  }

}
