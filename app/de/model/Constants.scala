package de.model

import enumeratum.EnumEntry
import enumeratum.Enum

sealed trait Fields extends EnumEntry
  object Fields extends Enum[Fields] {
    val values = findValues
  
    case object $in                                  extends Fields
    case object terms                                extends Fields
    case object sample_id                            extends Fields
    case object patient_barcode                      extends Fields
    case object sample_barcode                       extends Fields
    case object study_id                             extends Fields
    case object tissue                               extends Fields
    case object subtissue                            extends Fields
    case object definition                           extends Fields
    case object gender                               extends Fields
    case object race                                 extends Fields
    case object ethnicity                            extends Fields
    case object vital_status                         extends Fields
    case object age_normal_tissue_collected_in_years extends Fields
    case object disease                              extends Fields
    case object disease_name                         extends Fields
    case object disease_subtype                      extends Fields
    case object age_at_diagnosis_in_days             extends Fields
    case object event_free_survival_time_in_days     extends Fields
    case object overall_survival_time_in_days        extends Fields
  }