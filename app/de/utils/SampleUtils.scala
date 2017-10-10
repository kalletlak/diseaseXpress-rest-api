package de.utils

import io.swagger.annotations.ApiModel
import play.api.libs.json.{JsValue, Json, Writes}
import de.model.tags.Enums._
import de.model.tags.Sample._
import de.model.tags.{Sample, Tag, NormalSample, TumorSample}

// ===========================================================================
object SampleUtils {

  private val keyRegex =
    "[A-Za-z0-9_-]+".r
  
  private val valRegex =
    """[A-Za-z0-9_\-\/().>| *?#]+""".r

  // ---------------------------------------------------------------------------
  private def readTags(tagString: String): Seq[Tag] =
    if (tagString.isEmpty())
      Seq()
    else
      tagString
        .split(";", -1)
        .map { _.trim }
        .map {
          _
            .split("=")
            .map { _.trim }
              match {
                case Array(key: String, v: String) => // TODO: why safe?
                  require(keyRegex.pattern.matcher(key).matches())
                  require(valRegex.pattern.matcher(v).matches())
  
                  // TODO
                  val value =
                    try { Number(v) }
                    catch { case _: Throwable => Text(v) }
  
                  Tag(key, value)
                } }
        .toSeq

  // ---------------------------------------------------------------------------
  def readEnum[T](
          s: String,
          withNameOption: (String) => Option[T])
      : Either[T, unavailable] = {          
    assert(!s.isEmpty())
    
    withNameOption(s)
      match {
        case Some(x) => Left(x.asInstanceOf[T])
        case None    => Right(unavailable.withName(s))
      }
  }
  
  // ---------------------------------------------------------------------------  
  def readValue[Value](
          s: String,
          withNameOption: (String) => Value)
      : Either[Value, unavailable] = {    
    assert(!s.isEmpty())

    unavailable
      .withNameOption(s)
       match {
        case Some(x) => Right(x)
        case None    => Left(withNameOption(s))
      }
  }

  // ---------------------------------------------------------------------------
  def apply(line: String): Sample = {

    val spl = line.split("\t", -1).iterator

    val _sample_id                            = spl.next.trim
    
    val _patient_barcode                      = spl.next.trim
    val _sample_barcode                       = spl.next.trim
    val _group                                = spl.next.trim
    val _study                                = spl.next.trim
    val _disease                              = spl.next.trim
    val _disease_name                         = spl.next.trim
    val _disease_subtype                      = spl.next.trim
    val _tissue                               = spl.next.trim
    val _definition                           = spl.next.trim
    val _library_type                         = spl.next.trim
    val _platform                             = spl.next.trim
    val _center                               = spl.next.trim
    val _gender                               = spl.next.trim
    val _race                                 = spl.next.trim
    val _ethnicity                            = spl.next.trim
    val _age_normal_tissue_collected_in_years = spl.next.trim
    val _age_at_diagnosis_in_days             = spl.next.trim
    val _event_free_survival_time_in_days     = spl.next.trim
    val _overall_survival_time_in_days        = spl.next.trim
    val _vital_status                         = spl.next.trim
    
    val _tags: Seq[Tag] =
      readTags(spl.next.trim)

    group
      .withName(_group)
       match {
      
        case group.normals =>
          NormalSample(
            sample_id       = Text(_sample_id),
            
            patient_barcode = readValue(_patient_barcode, Text.apply),
            sample_barcode  = readValue(_sample_barcode, Text.apply),
            study           = Text(_study),
            tissue          = Text(_tissue),
            definition      = Text(_definition),
            library_type    = readEnum(_library_type, library_type.withNameOption),
            platform        = readEnum(_platform, platform.withNameOption),
            center          = Text(_center),
            gender          = readEnum(_gender, gender.withNameOption),
            race            = readEnum(_race, race.withNameOption),
            ethnicity       = readEnum(_ethnicity, ethnicity.withNameOption),
            vital_status    = readEnum(_vital_status, vital_status.withNameOption),
            
            tags = _tags,
            
            age_normal_tissue_collected_in_years = _age_normal_tissue_collected_in_years.toFloat)
        
        
        case group.tumors =>
          TumorSample(
            sample_id       = Text(_sample_id),
            
            patient_barcode = readValue(_patient_barcode, Text.apply),
            sample_barcode  = readValue(_sample_barcode, Text.apply),
            study           = Text(_study),
            tissue          = Text(_tissue),
            definition      = Text(_definition),
            library_type    = readEnum(_library_type, library_type.withNameOption),
            platform        = readEnum(_platform, platform.withNameOption),
            center          = Text(_center),
            gender          = readEnum(_gender, gender.withNameOption),
            race            = readEnum(_race, race.withNameOption),
            ethnicity       = readEnum(_ethnicity, ethnicity.withNameOption),
            vital_status    = readEnum(_vital_status, vital_status.withNameOption),
            
            tags = _tags,
            
            disease                          = Text(_disease),
            disease_name                     = Text(_disease_name),
            disease_subtype                  = Text(_disease_subtype),
            age_at_diagnosis_in_days         = readValue(_age_at_diagnosis_in_days, Number.apply),
            event_free_survival_time_in_days = readValue(_event_free_survival_time_in_days, Number.apply),
            overall_survival_time_in_days    = readValue(_overall_survival_time_in_days, Number.apply))     
        
        case _ =>
          assert(false, "Invalid group")
          ??? // FIXME     
        
      }
  }

  // ---------------------------------------------------------------------------
  implicit val writeJson =
    new Writes[Sample] {
    
      def writes(obj: Sample): JsValue = {
val tags =
          obj
            .tags
            .map { obj =>
              obj.key -> obj.value.formatJson }
            .toMap
        
        Json.toJson(obj.getAllTagsAsMap)
      }
      
    }

}
