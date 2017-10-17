package de.model.tags

import scala.math.BigDecimal.double2bigDecimal

import de.model.tags.Enums.{ ethnicity, gender, library_type, platform, race, unavailable, vital_status }
import play.api.libs.json.{ JsNumber, JsString, JsValue }
import Sample.{Text, EitherValue, Number}
import de.model.Fields

// ===========================================================================
trait Sample {
    
      val sample_id:       Text
      val patient_barcode: Either[Text,         unavailable]
      val sample_barcode:  Either[Text,         unavailable]
      val study_id:        Text
      val tissue:          Text
      val definition:      Text
      val library_type:    Either[library_type, unavailable]
      val platform:        Either[platform,     unavailable]
      val center:          Text
      val gender:          Either[gender,       unavailable]
      val race:            Either[race,         unavailable]
      val ethnicity:       Either[ethnicity,    unavailable]
      val vital_status:    Either[vital_status, unavailable]
      val tags:            Seq[Tag]

      // ---------------------------------------------------------------------------
      def getAllTags: Seq[Tag]
  
      def getAllTagsAsMap: Map[String, JsValue] =
        getAllTags
          .map { tag =>
            (tag.key -> tag.value.formatJson) }
          .toMap    
  
    }

    // ===========================================================================
    case class NormalSample(        
          override val sample_id: Text,
          override val patient_barcode: Either[Text,         unavailable],
          override val sample_barcode:  Either[Text,         unavailable],
          override val study_id:        Text,
          override val tissue:          Text,
          override val definition:      Text,
          override val library_type:    Either[library_type, unavailable],
          override val platform:        Either[platform,     unavailable],
          override val center:          Text,
          override val gender:          Either[gender,       unavailable],
          override val race:            Either[race,         unavailable],
          override val ethnicity:       Either[ethnicity,    unavailable],
          override val vital_status:    Either[vital_status, unavailable],
          override val tags:            Seq[Tag],

          age_normal_tissue_collected_in_years: Double)
                       
        extends Sample {

      def getAllTags() =
        Seq(
            Tag(Fields.sample_id.entryName,       sample_id),
            Tag(Fields.patient_barcode.entryName, EitherValue(patient_barcode)),
            Tag(Fields.sample_barcode.entryName,  EitherValue(sample_barcode)),
            Tag(Fields.study_id.entryName,        study_id),
            Tag(Fields.tissue.entryName,          tissue),
            Tag(Fields.definition.entryName,      definition),
            Tag(Fields.library_type.entryName,    EitherValue(library_type)),
            Tag(Fields.platform.entryName,        EitherValue(platform)),
            Tag(Fields.center.entryName,          center),
            Tag(Fields.gender.entryName,          EitherValue(gender)),
            Tag(Fields.race.entryName,            EitherValue(race)),
            Tag(Fields.ethnicity.entryName,       EitherValue(ethnicity)),
            Tag(Fields.vital_status.entryName,    EitherValue(vital_status)),

            Tag(Fields.age_normal_tissue_collected_in_years.entryName, Number(age_normal_tissue_collected_in_years.toString))) ++
          tags
  
    }
  
    // ---------------------------------------------------------------------------
    case class TumorSample(
        
          override val sample_id:       Text,
          override val patient_barcode: Either[Text, unavailable],
          override val sample_barcode:  Either[Text, unavailable],
          override val study_id:        Text,
          override val tissue:          Text,
          override val definition:      Text,
          override val library_type:    Either[library_type, unavailable],
          override val platform:        Either[platform, unavailable],
          override val center:          Text,
          override val gender:          Either[gender, unavailable],
          override val race:            Either[race, unavailable],
          override val ethnicity:       Either[ethnicity, unavailable],
          override val vital_status:    Either[vital_status, unavailable],
          override val tags:            Seq[Tag],
          
          disease:                          Text,
          disease_name:                     Text,
          disease_subtype:                  Text,
          age_at_diagnosis_in_days:         Either[Number, unavailable],
          event_free_survival_time_in_days: Either[Number, unavailable],
          overall_survival_time_in_days:    Either[Number, unavailable])

        extends Sample {

      def getAllTags() =
          Seq(
            Tag(Fields.sample_id.entryName,       sample_id),
            Tag(Fields.patient_barcode.entryName, EitherValue(patient_barcode)),
            Tag(Fields.sample_barcode.entryName,  EitherValue(sample_barcode)),
            Tag(Fields.study_id.entryName,        study_id),
            Tag(Fields.tissue.entryName,          tissue),
            Tag(Fields.definition.entryName,      definition),
            Tag(Fields.library_type.entryName,    EitherValue(library_type)),
            Tag(Fields.platform.entryName,        EitherValue(platform)),
            Tag(Fields.center.entryName,          center),
            Tag(Fields.gender.entryName,          EitherValue(gender)),
            Tag(Fields.race.entryName,            EitherValue(race)),
            Tag(Fields.ethnicity.entryName,       EitherValue(ethnicity)),
            Tag(Fields.vital_status.entryName,    EitherValue(vital_status)),
            
            Tag(Fields.disease.entryName,                          disease),
            Tag(Fields.disease_name.entryName,                     disease_name),
            Tag(Fields.disease_subtype.entryName,                  disease_subtype),
            Tag(Fields.age_at_diagnosis_in_days.entryName,         EitherValue(age_at_diagnosis_in_days)),
            Tag(Fields.event_free_survival_time_in_days.entryName, EitherValue(event_free_survival_time_in_days)),
            Tag(Fields.overall_survival_time_in_days.entryName,    EitherValue(overall_survival_time_in_days))) ++
        tags      
  
    }

    // ===========================================================================
    object Sample {
        
      trait Value extends Formatter
      
        // ---------------------------------------------------------------------------
        case class Text(value: String) extends Value {
        
          override val formatJson = JsString(value)
          //override val formatQuery = s""""$value""""
          
        }
      
        // ---------------------------------------------------------------------------
        case class Number(value: String) extends Value {
          
          private val internal: Double = value.toDouble
          
          override val formatJson = JsNumber(internal)
         // override val formatQuery = s"""$internal"""
          
        }
      
        // ---------------------------------------------------------------------------
        case class EitherValue(value: Either[Value, unavailable]) extends Value {
          
          override val formatJson = value.fold( l => l.formatJson , r => r.formatJson)
          //override val formatQuery = s"""$value"""
      
        }
        
    }
    
// ===========================================================================

