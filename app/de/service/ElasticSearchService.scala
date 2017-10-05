package de.service

import com.sksamuel.elastic4s.http.search.SearchHit

import de.model.Inputs.{ FilterUnit, InputDataModel }
import de.repository.Repository
import io.swagger.annotations.ApiModel
import play.api.libs.json.{ JsObject, Json }
import de.model.{ RsemGene, RsemIsoform, Abundance }

trait ElasticSearchService extends ServiceComponent {
  this: Repository =>

  def service = new DefaultService

  class DefaultService extends Service {
    override def find(collectionName: String,
                      filters: Seq[FilterUnit]) = dao.find(collectionName, filters)

    def getRsemGeneData(projection: InputDataModel,
                        filters: Seq[FilterUnit]): Iterable[RsemGene] = {

      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[SearchHit]].map { searchResult =>
          {

            val _data = Json.parse(searchResult.sourceAsString).asInstanceOf[JsObject]
            val gene_id = searchResult.sourceAsMap("gene_id").toString()
            RsemGene.readJson(
              gene_id,
              _data,
              projection).get
          }
        }

    }

    def getAbundanceData(projection: InputDataModel,
                         filters: Seq[FilterUnit]): Iterable[Abundance] = {

      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[SearchHit]].map { searchResult =>
          {

            val _data = Json.parse(searchResult.sourceAsString).asInstanceOf[JsObject]
            val transcript_id = searchResult.sourceAsMap("transcript_id").toString()
            Abundance.readJson(
              transcript_id,
              _data,
              projection).get
          }
        }

    }

    def getIsoformData(projection: InputDataModel,
                       filters: Seq[FilterUnit]): Iterable[RsemIsoform] = {

      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[SearchHit]].map { searchResult =>
          {

            val _data = Json.parse(searchResult.sourceAsString).asInstanceOf[JsObject]
            val transcript_id = searchResult.sourceAsMap("transcript_id").toString()
            RsemIsoform.readJson(
              transcript_id,
              _data,
              projection).get
          }
        }

    }

  }
}
