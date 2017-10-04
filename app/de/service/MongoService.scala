package de.service

import de.model.{ RsemGene, RsemIsoform, Abundance }
import de.model.DomainTypes.{ GeneId, TranscriptId }
import de.model.Inputs.{ FilterUnit, InputDataModel }
import de.repository.Repository
import de.utils.PlayJsonUtils.JsObjectImplicits
import io.swagger.annotations.ApiModel
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue.jsValueToJsLookup

trait MongoService extends ServiceComponent {
  this: Repository =>

  def service = new DefaultMongoService

  class DefaultMongoService extends Service {
    def find(collectionName: String,
             filters: Seq[FilterUnit]) = dao.find(collectionName, filters)

    def getRsemGeneData(projection: InputDataModel,
                        filters: Seq[FilterUnit]): Iterable[RsemGene] = {
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .flatMap { result =>
          {
            val gene_id = (result \ "gene_id").as[GeneId]
            result
              .parseObjectArray("data")
              .map { _data =>
                RsemGene.readJson(
                  gene_id,
                  _data,
                  projection).get
              }
          }
        }
    }

    def getAbundanceData(projection: InputDataModel,
                         filters: Seq[FilterUnit]): Iterable[Abundance] = {
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .flatMap { result =>
          {
            val transcript_id = (result \ "transcript_id").as[TranscriptId]
            result
              .parseObjectArray("data")
              .map { _data =>
                Abundance.readJson(
                  transcript_id,
                  _data,
                  projection).get
              }
          }
        }
    }

    def getIsoformData(projection: InputDataModel,
                       filters: Seq[FilterUnit]): Iterable[RsemIsoform] = {
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .flatMap { result =>
          {
            val transcript_id = (result \ "transcript_id").as[TranscriptId]
            result
              .parseObjectArray("data")
              .map { _data =>
                RsemIsoform.readJson(
                  transcript_id,
                  _data,
                  projection).get
              }
          }
        }

    }

  }
}
