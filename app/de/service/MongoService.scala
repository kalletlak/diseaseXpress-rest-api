package de.service

import de.model.DomainTypes.GeneId
import de.model.DomainTypes.TranscriptId
import de.model.input.{FilterUnit, InputDataModel}
import de.model.output.{Abundance, RsemGene, RsemIsoform}
import de.repository.Repository
import de.utils.PlayJsonUtils.JsObjectImplicits
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue.jsValueToJsLookup
import de.model.input.RsemIsoformProjectons
import de.model.input.AbundanceProjectons
import de.model.input.RsemGeneProjectons

// ===========================================================================
trait MongoService extends ServiceComponent {
  this: Repository =>

  def service = new DefaultMongoService

  // ---------------------------------------------------------------------------
  final class DefaultMongoService extends Service {
    
    def find(
          collectionName: String,
          filters:        Seq[FilterUnit]) =
        dao.find(collectionName, filters)

    // ---------------------------------------------------------------------------
    def getRsemGeneData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[RsemGene] =
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .map { result =>          
          
          RsemGene
                .fromJson(
                  result,
                  projection.asInstanceOf[RsemGeneProjectons])
                  }    

    // ---------------------------------------------------------------------------
    def getAbundanceData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[Abundance] =
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .map { result =>          
          
          Abundance
                .fromJson(
                  result,
                  projection.asInstanceOf[AbundanceProjectons]) 
                  }    

    // ---------------------------------------------------------------------------
    def getIsoformData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[RsemIsoform] =      
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .map { result =>          
          RsemIsoform
                .fromJson(
                  result,
                  projection.asInstanceOf[RsemIsoformProjectons]) }    

  }
}

// ===========================================================================
