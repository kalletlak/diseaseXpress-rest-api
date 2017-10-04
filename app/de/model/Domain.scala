package de.model

import scala.math.BigDecimal.double2bigDecimal

import de.model.Enums.{ Tag, ethnicity, gender, library_type, platform, race, study, unavailable, vital_status }
import io.swagger.annotations.ApiModel
import play.api.libs.json.{ JsNumber, JsString, JsValue }

object Domain {

  trait Formatter {
    val formatQuery: String
    val formatJson: JsValue
  }
  trait Value extends Formatter
  case class Text(value: String) extends Value {
    override val formatJson = JsString(value)
    override val formatQuery = s""""$value""""
  }
  case class Number(value: String) extends Value {
    private val internal = value.toDouble
    override val formatJson = JsNumber(internal)
    override val formatQuery = s"""$internal"""
  }

  case class EitherValue(value: Either[Value, unavailable]) extends Value {
    override val formatJson = value.fold( l => l.formatJson , r => r.formatJson)
    override val formatQuery = s"""$value"""
  }

  @ApiModel("SampleAnnotation")
  trait Sample {
    val sample_id: Text
    val patient_barcode: Either[Text, unavailable]
    val sample_barcode: Either[Text, unavailable]
    val study: study
    val tissue: Text
    val definition: Text
    val library_type: Either[library_type, unavailable]
    val platform: Either[platform, unavailable]
    val center: Text
    val gender: Either[gender, unavailable]
    val race: Either[race, unavailable]
    val ethnicity: Either[ethnicity, unavailable]
    val vital_status: Either[vital_status, unavailable]
    val tags: Seq[Tag]
    def getAllTags: Seq[Tag]
    def getAllTagsAsMap: Map[String, JsValue] = {
      getAllTags.map { tag => (tag.key -> tag.value.formatJson) }.toMap
    }

  }

  case class NormalSample(override val sample_id: Text,
                          override val patient_barcode: Either[Text, unavailable],
                          override val sample_barcode: Either[Text, unavailable],
                          override val study: study,
                          override val tissue: Text,
                          override val definition: Text,
                          override val library_type: Either[library_type, unavailable],
                          override val platform: Either[platform, unavailable],
                          override val center: Text,
                          override val gender: Either[gender, unavailable],
                          override val race: Either[race, unavailable],
                          override val ethnicity: Either[ethnicity, unavailable],
                          override val vital_status: Either[vital_status, unavailable],
                          override val tags: Seq[Tag],
                          age_normal_tissue_collected_in_years: Double) extends Sample {
    def getAllTags() = {
      val temp = Seq(Tag("sample_id", sample_id),
        Tag("patient_barcode", EitherValue(patient_barcode)),
        Tag("sample_barcode", EitherValue(sample_barcode)),
        Tag("study", Text(study.entryName)),
        Tag("tissue", tissue),
        Tag("definition", definition),
        Tag("library_type", EitherValue(library_type)),
        Tag("platform", EitherValue(platform)),
        Tag("center", center),
        Tag("gender", EitherValue(gender)),
        Tag("race", EitherValue(race)),
        Tag("ethnicity", EitherValue(ethnicity)),
        Tag("vital_status", EitherValue(vital_status)),
        Tag("age_normal_tissue_collected_in_years", Number(age_normal_tissue_collected_in_years.toString))) ++ tags
      temp
    }

  }

  case class TumorSample(override val sample_id: Text,
                         override val patient_barcode: Either[Text, unavailable],
                         override val sample_barcode: Either[Text, unavailable],
                         override val study: study,
                         override val tissue: Text,
                         override val definition: Text,
                         override val library_type: Either[library_type, unavailable],
                         override val platform: Either[platform, unavailable],
                         override val center: Text,
                         override val gender: Either[gender, unavailable],
                         override val race: Either[race, unavailable],
                         override val ethnicity: Either[ethnicity, unavailable],
                         override val vital_status: Either[vital_status, unavailable],
                         override val tags: Seq[Tag],
                         disease: Text,
                         disease_name: Text,
                         disease_subtype: Text,
                         age_at_diagnosis_in_days: Either[Number, unavailable],
                         event_free_survival_time_in_days: Either[Number, unavailable],
                         overall_survival_time_in_days: Either[Number, unavailable]) extends Sample {
    def getAllTags() = {
      val temp = Seq(Tag("sample_id", sample_id),
        Tag("patient_barcode", EitherValue(patient_barcode)),
        Tag("sample_barcode", EitherValue(sample_barcode)),
        Tag("study", Text(study.entryName)),
        Tag("tissue", tissue),
        Tag("definition", definition),
        Tag("library_type", EitherValue(library_type)),
        Tag("platform", EitherValue(platform)),
        Tag("center", center),
        Tag("gender", EitherValue(gender)),
        Tag("race", EitherValue(race)),
        Tag("ethnicity", EitherValue(ethnicity)),
        Tag("vital_status", EitherValue(vital_status)),
        Tag("disease", disease),
        Tag("disease_name", disease_name),
        Tag("disease_subtype", disease_subtype),
        Tag("age_at_diagnosis_in_days", EitherValue(age_at_diagnosis_in_days)),
        Tag("event_free_survival_time_in_days", EitherValue(event_free_survival_time_in_days)),
        Tag("overall_survival_time_in_days", EitherValue(overall_survival_time_in_days))) ++ tags
      temp
    }

  }
}
