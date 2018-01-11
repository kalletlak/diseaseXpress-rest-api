package de.service

import com.sksamuel.elastic4s.http.search.SearchHit

import de.model.input.{FilterUnit, InputDataModel}
import de.model.output.{Abundance, RsemGene, RsemIsoform}
import de.repository.Repository
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import de.model.input.RsemGeneProjectons
import de.model.input.AbundanceProjectons
import de.model.input.RsemIsoformProjectons

// ===========================================================================
trait ElasticSearchService extends ServiceComponent {
  this: Repository =>

  def service = new DefaultService

  // ---------------------------------------------------------------------------
  final class DefaultService extends Service {
    
    override def find(
        collectionName: String,
        filters:        Seq[FilterUnit]) =
      dao.find(collectionName, filters)

    // ---------------------------------------------------------------------------
    def getRsemGeneData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[RsemGene] =

      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[SearchHit]]
      .map { searchResult =>          
          val _data =
            Json
              .parse(searchResult.sourceAsString)
              .as[JsObject]
          
          RsemGene
            .fromJson(
              _data,
              projection.asInstanceOf[RsemGeneProjectons]) }
   

    // ---------------------------------------------------------------------------
    def getAbundanceData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[Abundance] =

      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[SearchHit]]
        .map { searchResult =>
          val _data =
            Json
              .parse(searchResult.sourceAsString)
              .as[JsObject]
          
          Abundance
            .fromJson(
              _data,
              projection.asInstanceOf[AbundanceProjectons]) }
    

    // ---------------------------------------------------------------------------
    def getIsoformData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[RsemIsoform] =

      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[SearchHit]]
        .map { searchResult =>
          val _data =
            Json
              .parse(searchResult.sourceAsString)
              .as[JsObject]

          RsemIsoform
            .fromJson(
              _data,
              projection.asInstanceOf[RsemIsoformProjectons]) }
    
  }
  
}

// ===========================================================================
