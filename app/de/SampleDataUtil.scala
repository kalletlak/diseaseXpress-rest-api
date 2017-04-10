package de

import java.io.InputStream

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
    val spl = line.split("\t")
    SampleAnnotation(spl(0), spl(1), spl(2), spl(3), spl(4), spl(5), spl(6), spl(7), spl(8), spl(9), spl(10), spl(11), spl(12))
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

}

