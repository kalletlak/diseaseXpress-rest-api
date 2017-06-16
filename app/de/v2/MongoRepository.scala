package de.v2

import scala.annotation.migration

import com.mongodb.MongoClient
import com.mongodb.MongoClientURI

import io.swagger.annotations.ApiModel
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue.jsValueToJsLookup
import de.v2.utils.PlayJsonUtils.JsObjectImplicits
import de.v2.model.Inputs._
import de.v2.utils.GeneDataUtil
import de.v2.model._

import de.v2.model.DomainTypes._
import de.v2.utils.Enums.Normalization
import de.v2.utils.Enums.IdQuery

case class MongoRepositoryConfig(
  uri: String,
  databaseName: String)

final class MongoRepository(val conf: MongoRepositoryConfig) {

  private val client: MongoClient =
    new MongoClient(
      new MongoClientURI(
        conf.uri))

  private val dao: MongoDAO =
    new MongoDAO(
      client,
      conf.databaseName)

  def findData(collectionName: String,
               filters: Map[Key, Seq[Value]]): Iterable[JsObject] = dao.findData(collectionName, filters)

  private def getRsemGeneData(input_filters: ObjectFilters,
                              input_gene_ids: Seq[GeneId],
                              study_ids: Seq[StudyId]): Map[(String, String), SampleRsemGeneOutput] = {

    val _filters = Map("gene_id" -> input_gene_ids, "study_id" -> study_ids)
    findData(input_filters.collection_name, _filters)
      .map { result =>
        {
          val gene_id = (result \ "gene_id").as[GeneId]
          result
            .parseObjectArray("data")
            .map { _data =>
              SampleRsemGeneOutput.readJson(
                gene_id,
                _data,
                input_filters)
            }
            .map { _.get }
        }
      }
      .flatten
      .map { sampleRsemGene => (sampleRsemGene.sample_id, sampleRsemGene.gene_id) -> sampleRsemGene }
      .toMap
  }

  private def getAbundanceData(input_filters: ObjectFilters,
                               input_transcript_ids: Seq[TranscriptId],
                               study_ids: Seq[StudyId]): Map[(String, String), SampleAbundanceOutput] = {
    val _filters = Map("transcript_id" -> input_transcript_ids, "study_id" -> study_ids)

    findData(input_filters.collection_name, _filters)
      .map { result =>
        {
          val transcript_id = (result \ "transcript_id").as[TranscriptId]
          result
            .parseObjectArray("data")
            .map { _data =>
              SampleAbundanceOutput.readJson(
                transcript_id,
                _data,
                input_filters)
            }
            .map { _.get }
        }
      }
      .flatten
      .map { sampleAbundance => (sampleAbundance.sample_id, sampleAbundance.transcript_id) -> sampleAbundance }
      .toMap
  }

  private def getIsoformData(input_filters: ObjectFilters,
                             input_transcript_ids: Seq[TranscriptId],
                             study_ids: Seq[StudyId]): Map[(String, String), SampleRsemIsoformOutput] = {
    val _filters = Map("transcript_id" -> input_transcript_ids, "study_id" -> study_ids)
    findData(input_filters.collection_name, _filters)
      .map { result =>
        {
          val transcript_id = (result \ "transcript_id").as[TranscriptId]
          result
            .parseObjectArray("data")
            .map { _data =>
              SampleRsemIsoformOutput.readJson(
                transcript_id,
                _data,
                input_filters)
            }
            .map { _.get }
        }
      }
      .flatten
      .map { sampleRsemIsoform => (sampleRsemIsoform.sample_id, sampleRsemIsoform.transcript_id) -> sampleRsemIsoform }
      .toMap

  }

  def getData(queryIdRef: IdQuery,
              ref_ids: Seq[GeneId],
              normalizations: Map[Normalization, ObjectFilters],
              study_ids: Seq[StudyId]): Seq[GeneLevelOutput] = {

    //get queried gene objects
    val genes: Seq[GeneInfoOutput] = (queryIdRef match {
      case IdQuery.GeneIdQuery => ref_ids
        .distinct
        .map { GeneIdQuery.apply }
      case IdQuery.GeneSymbolQuery => ref_ids
        .distinct
        .map { GeneSymbolQuery.apply }
      case IdQuery.TranscriptIdQuery => ref_ids
        .distinct
        .map { TranscriptIdQuery.apply }
    }).map { GeneDataUtil.getGeneInputRef }
      .filter(_.isDefined)
      .map(_.get)

    //get the unique geneId transcriptid's map
    val input_genes: Map[GeneId, (GeneSymbol, Seq[TranscriptId])] = genes
      .groupBy { _.gene_id }
      .mapValues { gene_info =>
        (gene_info.head.gene_symbol, gene_info
          .flatMap {
            _.transcripts
              .map { _.transcript_id }
          }
          .distinct)
      }

    val input_gene_ids = input_genes.keys.toSeq

    val input_transcript_ids = input_genes.values.flatMap { _._2 }.toSeq

    val _abundanceData: Map[(String, String), SampleAbundanceOutput] = normalizations.get(Normalization.sample_abundance) match {
      case Some(filter) => getAbundanceData(
        filter,
        input_transcript_ids,
        study_ids)
      case None => Map() // map it to empty Map if normalization is not passed in input query
    }

    val _isoformData: Map[(String, String), SampleRsemIsoformOutput] = normalizations.get(Normalization.sample_rsem_isoform) match {
      case Some(filter) => getIsoformData(
        filter,
        input_transcript_ids,
        study_ids)
      case None => Map() // map it to empty Map if normalization is not passed in input query
    }

    val _rsemData: Map[(String, String), SampleRsemGeneOutput] = normalizations.get(Normalization.rsem) match {
      case Some(filter) => getRsemGeneData(
        filter,
        input_gene_ids,
        study_ids)
      case None => Map() // map it to empty Map if normalization is not passed in input query
    }

    //join sample transcript data by transcript ids
    val _transcriptDataKeys = (_abundanceData.keySet ++ _isoformData.keySet)
      .toSeq
      .distinct

    val _trancriptData = _transcriptDataKeys
      .map {
        case (sample_id, transcript_id) => (sample_id, transcript_id) ->
          TranscriptLevelOutput(transcript_id,
            _abundanceData.get((sample_id, transcript_id)),
            _isoformData.get((sample_id, transcript_id)))
      }
      .toMap

    val _transcriptDataKeysTemp = _transcriptDataKeys.map {
      case (sample_id, _) => sample_id
    }

    val _rsemDataKeys = _rsemData.map {
      case ((sample_id, _), _) => sample_id
    }
    //these are the samples which should be same as input sample list but filtering out the samples which doesn't have data
    val targetSamples = (_transcriptDataKeysTemp ++ _rsemDataKeys)
      .toSeq
      .distinct

    //join sample gene rsem data with transcript data
    //TODO: need to find a better way
    //case(gene_id, (gene_symbol, transcript_ids))
    input_genes.map {
      case (gene_id, (gene_symbol, transcript_ids)) => {
        val gene_data = targetSamples.map { sample_id =>
          {
            val sampleRsemData = _rsemData.get((sample_id, gene_id))

            val sampleTranscriptData = transcript_ids
              .map { transcript_id => _trancriptData.get((sample_id, transcript_id)) }
              .filter { _.isDefined }
              .map { _.get }

            SampleDataOutput(sample_id, sampleRsemData, if (sampleTranscriptData.size > 0) Some(sampleTranscriptData) else None)
          }
        }
        GeneLevelOutput(gene_id, gene_symbol, gene_data)

      }
    }.toSeq

  }
}
