package de.v2.utils

import java.io.InputStream
import play.api.libs.json.Json
import play.api.libs.json.Writes
import play.api.libs.json.JsValue
import io.swagger.annotations.ApiModel
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import de.v2.model.DomainTypes.StudyId

@ApiModel("SampleAnnotation")
case class SampleAnnotation(
    @ApiModelProperty(name = "sample_id") analysis_id: String,
    patient_barcode: String,
    sample_barcode: String,
    group: String,
    study: String,
    disease: String,
    disease_name: String,
    disease_subtype: String,
    tissue: String,
    definition: String,
    library_type: String,
    platform: String,
    center: String) {
  @ApiModelProperty(hidden = true)
  val values =
    Seq(analysis_id,
      patient_barcode,
      sample_barcode,
      group,
      study,
      disease,
      disease_name,
      disease_subtype,
      tissue,
      definition,
      library_type,
      platform,
      center)

}

object SampleAnnotation {

  val header = Seq("sample_id",
    "patient_barcode",
    "sample_barcode",
    "group", "study",
    "disease",
    "disease_name",
    "disease_subtype",
    "tissue",
    "definition",
    "library_type",
    "platform",
    "center")

  def apply(line: String): SampleAnnotation = {
    val spl = line.split("\t", -1).iterator
    SampleAnnotation(analysis_id = spl.next,
      patient_barcode = spl.next,
      sample_barcode = spl.next,
      group = spl.next,
      study = spl.next,
      disease = spl.next,
      disease_name = spl.next,
      disease_subtype = spl.next,
      tissue = spl.next,
      definition = spl.next,
      library_type = spl.next,
      platform = spl.next,
      center = spl.next)
  }

  implicit val writeJson = new Writes[SampleAnnotation] {
    def writes(obj: SampleAnnotation): JsValue = {
      Json.obj(
        "sample_id" -> obj.analysis_id,
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
        "center" -> obj.center)
    }
  }

}

object SampleDataUtil {
  val studySampleMap: Map[String, Seq[SampleAnnotation]] = {
    val stream: InputStream = getClass.getResourceAsStream("/sample_info_19262.txt")
    val src = scala.io.Source.fromInputStream(stream)
    val x = src.getLines
      .drop(1)
      .map { SampleAnnotation.apply }
      .toList
      .groupBy { _.study }
    src.close()
    x
  }

  def getStudies(): Seq[StudyId] = studySampleMap.keys.toSeq

  def getSamples(studies: Seq[String] = Nil): Seq[String] = studies match {
    case Nil => studySampleMap
      .map { case (_, sampleAnnotations) => sampleAnnotations.map { _.analysis_id } }
      .flatten
      .toSeq
    case _ => studies
      .flatMap(studySampleMap) // get samples for each study and flatten it all up
      .map(_.analysis_id)
      .toSeq
  }

  def getSamplesInfo(studies: Seq[String] = Nil) = studies match {
    case Nil => studySampleMap
      .values
      .flatten
      .toSeq
    case _ => studies
      .flatMap(study_id => studySampleMap.getOrElse(study_id, Seq())) // get samples for each study and flatten it all up
      .toSeq
  }

}
