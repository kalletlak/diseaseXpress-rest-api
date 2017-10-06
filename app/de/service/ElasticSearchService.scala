package de.service

import com.sksamuel.elastic4s.http.search.SearchHit

import de.model.Inputs.FilterUnit
import de.model.Inputs.InputDataModel
import de.model.output.{Abundance, RsemGene, RsemIsoform}
import de.repository.Repository
import io.swagger.annotations.ApiModel
import play.api.libs.json.JsObject
import play.api.libs.json.Json

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
          
          val gene_id =
            searchResult
              .sourceAsMap("gene_id") // TODO: create enums for those
              .toString
          
          RsemGene
            .readJson(
              gene_id,
              _data,
              projection)
            .get }
   

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
          
          val transcript_id =
            searchResult
              .sourceAsMap("transcript_id")
              .toString // TODO: don't use toString or comment why safe (not so by default)

          Abundance
            .readJson(
              transcript_id,
              _data,
              projection)
            .get }
    

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

          val transcript_id =
            searchResult
            .sourceAsMap("transcript_id")
            .toString
          
          RsemIsoform
            .readJson(
              transcript_id,
              _data,
              projection)
            .get }
    
  }
  
}

// ===========================================================================
