package de.controllers

import de.model.output.GeneData
import de.model.Inputs.InputFilters
import de.service.Service
import de.utils.Enums.{Normalization, Projection}
import play.api.libs.json.{JsObject, Json, JsString}

// ===========================================================================
object GenomicDataHelper {
    
  def apply
      (repo:              Service,
       filters:           InputFilters,
       ref_ids:           Either[Seq[String], Seq[String]],
       normalizationsOpt: Option[String],
       projectionOpt:     Option[String]) = {
    
    // TODO: replace two-step process:
    
    val projection_enum = this.projection_enum(projectionOpt)
    
    val normalization_enums = this.normalization_enums(normalizationsOpt)
    
    val norm_invalid: Map[String, Option[Normalization]] =
      normalization_enums
        .filter { case (_, norm_enum) =>
          norm_enum.isEmpty }

    if (norm_invalid.size > 0 || projection_enum.isEmpty)
      Left(errorObject(norm_invalid, projection_enum))
      
    else {            
          
      val start = System.currentTimeMillis()
      
      val genes: Seq[GeneData] =
        repo
          .getData(
            filters,
            normalizations =
              EnumCombo(
                projection_enum.get,
                normalizations(normalization_enums))
              .toMap)

      // TODO: to proper logging
      println("Total Execution(ms) : " + (System.currentTimeMillis() - start))

      Right(genes)
    }

  }
    
  // ===========================================================================
  // all these below will change once we separate input validation
  
  def projection_enum(projectionOpt: Option[String]): Option[Projection] =
    projectionOpt match {
      case Some(projection) => Projection.withNameOption(projection)
      case None             => Some(Projection.summary)
    }
  
  // ---------------------------------------------------------------------------
  def normalization_enums(normalizationsOpt: Option[String]): Map[String, Option[Normalization]] =
    normalizationsOpt match {
    
      case Some(normalizations) =>
        normalizations
          .split(",", -1)
          .map(normalization =>
            normalization ->
              Normalization.withNameOption(normalization))
          .toMap

      case None =>
        Map(
          Normalization.rsem               .entryName -> Some(Normalization.rsem),
          Normalization.sample_abundance   .entryName -> Some(Normalization.sample_abundance),
          Normalization.sample_rsem_isoform.entryName -> Some(Normalization.sample_rsem_isoform) )

    }

  // ---------------------------------------------------------------------------
  def normalizations(normalizationsOpt: Option[String]): Seq[Normalization] =
    normalizations(normalization_enums(normalizationsOpt))

  // ---------------------------------------------------------------------------
  def normalizations(data: Map[String, Option[Normalization]]): Seq[Normalization] =
    data.values.map(_.get).toSeq
    
  // ---------------------------------------------------------------------------
  private def errorObject(
      norm_invalid: Map[String, Option[Normalization]],
      projection_enum: Option[Projection])
      : JsObject = {
    
    var obj = Json.obj()
    
    // TODO: use JsObject rather
    
    obj =
      if (norm_invalid.size > 0) {  
        obj + ("normalizations" -> JsString(norm_invalid
          .keySet
          .mkString(
            "",
            ",",
            " , are invalid. Must be in [rsem, sample_abundance, sample_rsem_isoform]"))) }
      else
        Json.obj()

    if (projection_enum.isEmpty) {
      obj + ("projection" -> JsString(norm_invalid
        .keySet
        .mkString(
          "",
          ",",
          " , are invalid. Must be in [rsem, sample_abundance, sample_rsem_isoform]"))) }
    else
      Json.obj()
  }      
  
}

// ===========================================================================
