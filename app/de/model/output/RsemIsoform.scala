package de.model.output

import de.model.Inputs.{InputDataModel, RsemIsoformProjectons}
import de.utils.Enums.Projection
import de.utils.JsObjectWithOption
import de.utils.NumberUtils.DoubleImplicits
import de.utils.PlayJsonUtils.JsObjectImplicits
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json._
import play.api.libs.json.JsValue.jsValueToJsLookup
import com.datastax.driver.core.{Row => CassandraRow}

// ===========================================================================
@ApiModel("RsemIsoformData")
case class RsemIsoform( // TODO: separate out
    
     // initialized parameters with default values. This would be used in getting tsv format data
     // when querying for a particular normalization and doesn't have any data in the database
    
     transcript_id: String = "",
     
     sample_id:     String = "",
     
     @ApiModelProperty(
       dataType = "double",
       required = false)
     length: Option[Double] = None,
     
     @ApiModelProperty(
       dataType = "double",
       required = false)
     effective_length: Option[Double] = None,
     
     @ApiModelProperty(
       dataType = "double",
       required = false)
     expected_count: Option[Double] = None,
     
     @ApiModelProperty(
       dataType = "double",
       required = false)
     tpm: Option[Double] = None,
     
     @ApiModelProperty(
       dataType = "double",
       required = false)
     fpkm: Option[Double] = None,
     
     @ApiModelProperty(
       dataType = "double",
       required = false)
     isoform_percentage: Option[Double] = None)

  // ===========================================================================
  object RsemIsoform {
  
    def readJson(
            transcript_id:    String,
            obj:              JsObject,
            projectionFileds: InputDataModel = new RsemIsoformProjectons)
        : JsResult[RsemIsoform] = {

      val projectionFiledsObj = projectionFileds.asInstanceOf[RsemIsoformProjectons]
      
      JsSuccess(
        RsemIsoform(
          transcript_id = transcript_id,
          
          sample_id =
            (obj \ "sample_id")
              .as[String],

          length =
            obj
              .parseDoubleOption(
                "length",
                projectionFiledsObj.length),

          effective_length =
            obj
              .parseDoubleOption(
                "effective_length",
                projectionFiledsObj.effective_length),

          expected_count =
            obj
              .parseDoubleOption(
                "expected_count",
                projectionFiledsObj.expected_count),
                
          tpm =
            obj
              .parseDoubleOption(
                "tpm",
                projectionFiledsObj.tpm),
                
          fpkm =
            obj
              .parseDoubleOption(
                "fpkm",
                projectionFiledsObj.fpkm),
                
          isoform_percentage =
            obj
              .parseDoubleOption(
                "isoform_percentage",
                projectionFiledsObj.isoform_percentage)))  
    }
    
    // ---------------------------------------------------------------------------
    def readRow(
            row:              CassandraRow,
            projectionFileds: InputDataModel = new RsemIsoformProjectons())
        : JsResult[RsemIsoform] = {
  
      val projectionFiledsObj = projectionFileds.asInstanceOf[RsemIsoformProjectons]

      JsSuccess(
        RsemIsoform(
          transcript_id =
            row
              .getString("transcript_id"),
              
          sample_id =
            row
              .getString("sample_id"),
              
          length =
            row
              .getFloat("length")
              .parseDoubleOption(projectionFiledsObj.length),
              
          effective_length =
            row
              .getFloat("effective_length")
              .parseDoubleOption(projectionFiledsObj.effective_length),
              
          expected_count =
            row
              .getFloat("expected_count")
              .parseDoubleOption(projectionFiledsObj.expected_count),
              
          tpm =
            row
              .getFloat("tpm")
              .parseDoubleOption(projectionFiledsObj.tpm),
              
          fpkm =
            row
              .getFloat("fpkm")
              .parseDoubleOption(projectionFiledsObj.fpkm),

          isoform_percentage =
            row
              .getFloat("isoform_percentage")
              .parseDoubleOption(projectionFiledsObj.isoform_percentage)))
    }
  
    // ---------------------------------------------------------------------------
    implicit val writeJson = new Writes[RsemIsoform] {
      def writes(obj: RsemIsoform): JsValue =
        JsObjectWithOption(
          "length" ->
            Right(obj.length.map(Json.toJson(_))),

          "effective_length" ->
            Right(obj.effective_length.map(Json.toJson(_))),
            
          "expected_count" ->
            Right(obj.expected_count.map(Json.toJson(_))),
            
          "tpm" ->
            Right(obj.tpm.map(Json.toJson(_))),
          
          "fpkm" ->
            Right(obj.fpkm.map(Json.toJson(_))),
          
          "isoform_percentage" ->
            Right(obj.isoform_percentage.map(Json.toJson(_)))) }    
  
    // ---------------------------------------------------------------------------
    def getHeader(projection: Projection) =
      projection match {
      
        // TODO: as enum
        case Projection.summary =>
          Seq(
            "tpm")
  
        case Projection.detailed =>
          Seq(
            "length",
            "effective_length",
            "expected_count",
            "tpm",
            "fpkm",
            "isoform_percentage")
            
      }
  
    // ---------------------------------------------------------------------------
    def getValues(
        obj: RsemIsoform,
        projection: Projection) = // FIXME: not returning Anys...
      projection match {
  
        case Projection.summary =>
          Seq(
            obj.tpm.getOrElse(""))
  
        case Projection.detailed =>
          Seq(
            obj.length            .getOrElse(""),
            obj.effective_length  .getOrElse(""),
            obj.expected_count    .getOrElse(""),
            obj.tpm               .getOrElse(""),
            obj.fpkm              .getOrElse(""),
            obj.isoform_percentage.getOrElse(""))
  
      }

  }

// ===========================================================================
