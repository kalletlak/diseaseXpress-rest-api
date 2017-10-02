package de.model

import de.model.Domain.Value
import enumeratum.{ Enum, EnumEntry }
import play.api.libs.json.{ JsString, Json }
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Writes

object Enums {
  sealed trait unavailable extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object unavailable extends Enum[unavailable] {
    val values = findValues
    case object `unavailable` extends unavailable
  }

  sealed trait platform extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object platform extends Enum[platform] {
    val values = findValues
    case object `Illumina` extends platform
  }

  sealed abstract class library_type(override val entryName: String) extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object library_type extends Enum[library_type] {

    val values = findValues

    case object `ran_seq` extends library_type("RNA-Seq")
  }

  sealed trait study extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object study extends Enum[study] {
    val values = findValues
    case object TCGA extends study
    case object GTEx extends study
    case object TARGET extends study
    case object PNOC extends study
    case object CBTTC extends study
  }

  sealed trait gender extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object gender extends Enum[gender] {
    val values = findValues
    case object male extends gender
    case object female extends gender
  }

  sealed trait group extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object group extends Enum[group] {
    val values = findValues
    case object tumors extends group
    case object normals extends group
  }

  sealed abstract class race(override val entryName: String) extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object race extends Enum[race] {

    val values = findValues

    case object `white` extends race("white")
    case object `black_or_african_american` extends race("black or african american")
    case object `native_hawaiian_or_other_pacific_islander` extends race("native hawaiian or other pacific islander")
    case object `asian` extends race("asian")
    case object `other` extends race("other")
    case object `american_indian_or_alaska_native` extends race("american indian or alaska native")
  }

  sealed abstract class ethnicity(override val entryName: String) extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object ethnicity extends Enum[ethnicity] {

    val values = findValues

    case object `not_hispanic_or_latino` extends ethnicity("not hispanic or latino")
    case object `hispanic_or_latino` extends ethnicity("hispanic or latino")
  }
  case class Tag(key: String, value: Value)
  object Tag {
    implicit val tagWrites = new Writes[Tag] {
      def writes(tag: Tag) = Json.obj(
        "key" -> tag.key,
        "value" -> tag.value.formatJson)
    }
  }

  sealed trait vital_status extends EnumEntry with Value {
    override val formatJson = JsString(entryName)
    override val formatQuery = s""""entryName""""
  }

  object vital_status extends Enum[vital_status] {
    val values = findValues
    case object dead extends vital_status
    case object alive extends vital_status
  }
}
