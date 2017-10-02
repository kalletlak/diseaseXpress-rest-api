package de.service

import de.model.{ SampleAbundanceOutput, SampleRsemGeneOutput, SampleRsemIsoformOutput }
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
                        filters: Seq[FilterUnit]): Iterable[SampleRsemGeneOutput] = {
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .flatMap { result =>
          {
            val gene_id = (result \ "gene_id").as[GeneId]
            result
              .parseObjectArray("data")
              .map { _data =>
                SampleRsemGeneOutput.readJson(
                  gene_id,
                  _data,
                  projection).get
              }
          }
        }
    }

    def getAbundanceData(projection: InputDataModel,
                         filters: Seq[FilterUnit]): Iterable[SampleAbundanceOutput] = {
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .flatMap { result =>
          {
            val transcript_id = (result \ "transcript_id").as[TranscriptId]
            result
              .parseObjectArray("data")
              .map { _data =>
                SampleAbundanceOutput.readJson(
                  transcript_id,
                  _data,
                  projection).get
              }
          }
        }
    }

    def getIsoformData(projection: InputDataModel,
                       filters: Seq[FilterUnit]): Iterable[SampleRsemIsoformOutput] = {
      find(projection.collection_name, filters)
        .asInstanceOf[Iterable[JsObject]]
        .flatMap { result =>
          {
            val transcript_id = (result \ "transcript_id").as[TranscriptId]
            result
              .parseObjectArray("data")
              .map { _data =>
                SampleRsemIsoformOutput.readJson(
                  transcript_id,
                  _data,
                  projection).get
              }
          }
        }

    }

  }
}
