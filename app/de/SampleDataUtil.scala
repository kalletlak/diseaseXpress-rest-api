package de

import java.io.InputStream
import play.api.libs.json.Json

case class SampleAnnotation(
  analysis_id: String,
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
  center: String)

object SampleAnnotation {

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
  
}

object SampleDataUtil {
  val studySampleMap: Map[String, Seq[SampleAnnotation]] = {
    val stream: InputStream = getClass.getResourceAsStream("/sample_info_19262.txt")
    val src = scala.io.Source.fromInputStream(stream)
    val x = src.getLines.drop(1).map { SampleAnnotation.apply }.toList.groupBy { _.study }
    src.close()
    x
  }

  def getStudies(): Seq[String] = studySampleMap.keys.toSeq

  def getSamples(studies: Seq[String] = Nil): Seq[String] = studies match {
    case Nil => { studySampleMap.map(x => x._2.map { _.analysis_id }).flatten.toSeq }
    case _ => studies
      .flatMap(studySampleMap) // get samples for each study and flatten it all up
      .map(_.analysis_id)
      .toSeq
  }

  def getSamplesInfo(studies: Seq[String] = Nil) = studies match {
    case Nil => { studySampleMap.map(x => x._2).flatten.toSeq }
    case _ => studies
      .flatMap(studySampleMap) // get samples for each study and flatten it all up
      .toSeq
  }

}

