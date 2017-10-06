package de.model.output

import de.model.input.{InputDataModel, RsemGeneProjectons}
import de.utils.Enums.Projection
import de.utils.JsObjectWithOption
import de.utils.NumberUtils.DoubleImplicits
import de.utils.PlayJsonUtils.JsObjectImplicits
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json._
import play.api.libs.json.JsValue.jsValueToJsLookup
import com.datastax.driver.core.{Row => CassandraRow}

// ===========================================================================
@ApiModel("RsemGeneData")
case class RsemGene(
    
    // initialized parameters with default values. used would be used when getting tsv format data
      
    gene_id:   String = "",
    
    sample_id: String = "",
    
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
    fpkm: Option[Double] = None)

  // ===========================================================================
  object RsemGene {
  
    def fromJson(
            gene_id:             String,
            obj:                 JsObject,
            projectionFiledsObj: RsemGeneProjectons = new RsemGeneProjectons()) =
        RsemGene(
          gene_id = gene_id,
            
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
                projectionFiledsObj.fpkm) )
      
  
    // ---------------------------------------------------------------------------
    def fromCassandra
        (projectionFiledsObj: RsemGeneProjectons = new RsemGeneProjectons())
        (row:                 CassandraRow) =
      RsemGene(
        gene_id =
          row
            .getString("gene_id"),
            
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
            .parseDoubleOption(projectionFiledsObj.fpkm) )    
  
            
    // ---------------------------------------------------------------------------
    implicit val writeJson = new Writes[RsemGene] {
      
      def writes(obj: RsemGene): JsValue =
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
            Right(obj.fpkm.map(Json.toJson(_))))
      
    }
  
    // ---------------------------------------------------------------------------
    def getHeader(projection: Projection) = projection match {

      case Projection.summary =>
        Seq(
          "fpkm")
      
      case Projection.detailed =>
        Seq(
          "length",
          "effective_length",
          "expected_count",
          "tpm",
          "fpkm")
        
    }
  
    // ---------------------------------------------------------------------------
    def getValues(
        obj:        RsemGene,
        projection: Projection) =
      projection
       match {
      
        case Projection.summary =>
          Seq(
            obj.fpkm.getOrElse(""))
          
        case Projection.detailed =>
          Seq(
            obj.length          .getOrElse(""),
            obj.effective_length.getOrElse(""),
            obj.expected_count  .getOrElse(""),
            obj.tpm             .getOrElse(""),
            obj.fpkm            .getOrElse(""))
            
      }
  }

// ===========================================================================
