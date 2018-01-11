package de.model.output

import enumeratum.{Enum, EnumEntry, PlayJsonEnum}
import de.model.input.{InputDataModel, AbundanceProjectons}
import de.utils.Enums.Projection
import de.utils.JsObjectWithOption
import de.utils.NumberUtils.DoubleImplicits
import de.utils.PlayJsonUtils.JsObjectImplicits
import play.api.libs.json._
import play.api.libs.json.JsValue.jsValueToJsLookup
import com.datastax.driver.core.{Row => CassandraRow}

// ===========================================================================
case class Abundance(
    // initialized parameters with default values. used would be used when getting tsv format data
    transcript_id:    String         = "",
    sample_id:        String         = "",
    length:           Option[Double] = None,
    effective_length: Option[Double] = None,
    expected_count:   Option[Double] = None,
    tpm:              Option[Double] = None)

  // ===========================================================================
  object Abundance {
  
    sealed trait Fields extends EnumEntry
      object Fields extends Enum[Fields] {
        val values = findValues
      
        case object transcript_id    extends Fields
        case object sample_id        extends Fields
        case object length           extends Fields
        case object effective_length extends Fields
        case object expected_count   extends Fields
        case object tpm              extends Fields
      }
      
    import Fields._

    // ---------------------------------------------------------------------------
    def fromJson(
        obj:                 JsObject,
        projectionFiledsObj: AbundanceProjectons = new AbundanceProjectons) =
  
      Abundance(
        transcript_id = (obj \ transcript_id.entryName)
            .as[String],

        sample_id =
          (obj \ sample_id.entryName)
            .as[String],

        length =
          obj
            .parseDoubleOption(
              length.entryName,
              projectionFiledsObj.length),

        effective_length =
          obj
            .parseDoubleOption(
              effective_length.entryName,
              projectionFiledsObj
                .effective_length),

        expected_count =
          obj
            .parseDoubleOption(
              expected_count.entryName,
              projectionFiledsObj.expected_count),

        tpm =
          obj
            .parseDoubleOption(
              tpm.entryName,
              projectionFiledsObj.tpm) )    
  
    // ---------------------------------------------------------------------------
    def fromCassandra
          (projectionFiledsObj: AbundanceProjectons = new AbundanceProjectons())
          (row:                 CassandraRow) =  
        Abundance(
          transcript_id =
            row
              .getString(transcript_id.entryName),

          sample_id =
            row
              .getString(sample_id.entryName),

          length =
            row
              .getFloat(length.entryName)
              .parseDoubleOption(projectionFiledsObj.length),

          effective_length =
            row
              .getFloat(effective_length.entryName)
              .parseDoubleOption(projectionFiledsObj.effective_length),

          expected_count =
            row
              .getFloat(expected_count.entryName)
              .parseDoubleOption(projectionFiledsObj.expected_count),

          tpm =
            row
              .getFloat(tpm.entryName)
              .parseDoubleOption(projectionFiledsObj.tpm) )    
  
    // ---------------------------------------------------------------------------
    implicit val writeJson = new Writes[Abundance] {
      
      def writes(obj: Abundance): JsValue =
        JsObjectWithOption(

          length.entryName ->
            Right(obj.length.map(Json.toJson(_))),

          effective_length.entryName ->
            Right(obj.effective_length.map(Json.toJson(_))),

          expected_count.entryName ->
            Right(obj.expected_count.map(Json.toJson(_))),

          tpm.entryName ->
            Right(obj.tpm.map(Json.toJson(_))) )
      
    }
  
    // ---------------------------------------------------------------------------
    def getHeader(projection: Projection) = projection match {
      
      case Projection.summary =>
        Seq(tpm.entryName)

      case Projection.detailed =>
        Seq(
            length,
            effective_length,
            expected_count,
            tpm)
          .map(_.entryName)

    }
  
    // ---------------------------------------------------------------------------
    def getValues(
        obj:        Abundance,
        projection: Projection) =
      projection
       match {

        case Projection.summary =>
          Seq(
            obj.tpm.getOrElse(""))
        
        case Projection.detailed =>
          Seq(
            obj.length          .getOrElse(""),
            obj.effective_length.getOrElse(""),
            obj.expected_count  .getOrElse(""),
            obj.tpm             .getOrElse(""))
          
      }
  
  }

// ===========================================================================
