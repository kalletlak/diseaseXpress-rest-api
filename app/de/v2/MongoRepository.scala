package de.v2

import scala.annotation.migration

import com.mongodb.MongoClient
import com.mongodb.MongoClientURI

import de.v2.utils.PlayJsonUtils.JsObjectImplicits
import io.swagger.annotations.ApiModel
import play.api.libs.json.JsValue.jsValueToJsLookup
import de.v2.model.Inputs._
import de.v2.utils.GeneDataUtil
import de.v2.model._
import play.api.libs.json.JsObject
import de.v2.model.DomainTypes._

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

  def getDataByGeneSymbols(ref_ids: Seq[GeneSymbol],
                           normalizations: Seq[ObjectFilters],
                           targetSampleIds: Seq[SampleId]): Seq[GeneLevelOutput] = {

    val genes: Seq[GeneInfoOutput] = ref_ids
      .distinct
      .map { GeneSymbolQuery.apply }
      .map { GeneDataUtil.getGeneInputRef }
      .filter(_.isDefined)
      .map(_.get)
    getData(genes, normalizations, targetSampleIds)

  }

  def getDataByGeneIds(ref_ids: Seq[GeneId],
                       normalizations: Seq[ObjectFilters],
                       targetSampleIds: Seq[SampleId]): Seq[GeneLevelOutput] = {

    val genes: Seq[GeneInfoOutput] = ref_ids
      .distinct
      .map { GeneIdQuery.apply }
      .map { GeneDataUtil.getGeneInputRef }
      .filter(_.isDefined)
      .map(_.get)
    getData(genes, normalizations, targetSampleIds)

  }

  def getDataByTranscriptIds(ref_ids: Seq[TranscriptId],
                             normalizations: Seq[ObjectFilters],
                             targetSampleIds: Seq[SampleId]): Seq[GeneLevelOutput] = {

    val genes: Seq[GeneInfoOutput] = ref_ids
      .distinct
      .map { TranscriptIdQuery.apply }
      .map { GeneDataUtil.getGeneInputRef }
      .filter(_.isDefined)
      .map(_.get)
    //genes might have multiple records with same gene id
    getData(genes, normalizations, targetSampleIds)

  }
  def getAbundanceData(input_filters: Option[ObjectFilters], input_transcript_ids: Seq[TranscriptId], study_ids: Seq[StudyId]) = {
    input_filters match {
      case Some(_normalization_input: SampleAbundanceProjectons) => {

        val _filters = Map("transcript_id" -> input_transcript_ids, "study_id" -> study_ids)

        findData(_normalization_input.collection_name, _filters)
          .map { _data =>
            {
              val transcript_id = (_data \ "transcript_id").as[TranscriptId]
              _data
                .parseObjectArray("data")
                .map { _obj =>
                  SampleAbundanceOutput.readJson(
                    transcript_id = transcript_id,
                    obj = _obj,
                    projectionFileds = _normalization_input)
                }
                .map { _.get }
            }
          }
          .flatten
          .toSeq

      }
      case _ => Seq()
    }
  }

  def getIsoformData(input_filters: Option[ObjectFilters], input_transcript_ids: Seq[TranscriptId], study_ids: Seq[StudyId]) = {
    input_filters match {
      case Some(_normalization_input: SampleRsemIsoformProjectons) => {

        val _filters = Map("transcript_id" -> input_transcript_ids, "study_id" -> study_ids)
        findData(_normalization_input.collection_name, _filters)
          .map { _data =>
            {
              val transcript_id = (_data \ "transcript_id").as[TranscriptId]
              _data
                .parseObjectArray("data")
                .map { _obj =>
                  SampleRsemIsoformOutput.readJson(
                    transcript_id = transcript_id,
                    obj = _obj,
                    projectionFileds = _normalization_input)
                }
                .map { _.get }
            }
          }
          .flatten
          .toSeq

      }
      case _ => Seq()
    }
  }

  def getRsemGeneData(input_filters: Option[ObjectFilters], input_gene_ids: Seq[GeneId], study_ids: Seq[StudyId]) = {

    input_filters match {
      case Some(_normalization_input: SampleRsemGeneProjectons) => {

        val _filters = Map("gene_id" -> input_gene_ids, "study_id" -> study_ids)
        findData(_normalization_input.collection_name, _filters)
          .map { _data =>
            {
              val gene_id = (_data \ "gene_id").as[GeneId]
              _data
                .parseObjectArray("data")
                .map { _obj =>
                  SampleRsemGeneOutput.readJson(
                    gene_id = gene_id,
                    obj = _obj,
                    projectionFileds = _normalization_input)
                }
                .map { _.get }
            }
          }
          .flatten
          .toSeq

      }
      case _ => Seq()
    }
  }

  def getData(genes: Seq[GeneInfoOutput],
              data_types: Seq[ObjectFilters],
              study_ids: Seq[StudyId]): Seq[GeneLevelOutput] = {

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

    //get unique collections
    val normalizations = data_types
      .groupBy { _.collection_name }
      .mapValues { _.head }

    val input_gene_ids = input_genes.keys.toSeq

    val input_transcript_ids = input_genes.values.flatMap { _._2 }.toSeq

    val _abundanceData = getAbundanceData(
      normalizations.get("transcript_abundance"),
      input_transcript_ids,
      study_ids)
      .map { sampleAbundance => (sampleAbundance.sample_id, sampleAbundance.transcript_id) -> sampleAbundance }
      .toMap

    val _isoformData = getIsoformData(
      normalizations.get("transcript_isoform"),
      input_transcript_ids,
      study_ids)
      .map { sampleRsemIsoform => (sampleRsemIsoform.sample_id, sampleRsemIsoform.transcript_id) -> sampleRsemIsoform }
      .toMap

    val _rsemData = getRsemGeneData(
      normalizations.get("gene_rsem"),
      input_gene_ids,
      study_ids)
      .map { sampleRsemGene => (sampleRsemGene.sample_id, sampleRsemGene.gene_id) -> sampleRsemGene }
      .toMap

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

    //TODO: should updated this??
    //these are the samples which should be same as input sample list but filtering out the samples which doesn't have data
    val targetSamples = (_transcriptDataKeys.map { case (sample_id, _) => sample_id } ++ _rsemData.map { case ((sample_id, _), _) => sample_id })
      .toSeq
      .distinct

    //join sample gene rsem data with transcript data
    //TODO: need to find a better way
    genes.map { gene =>
      {
        val gene_data = targetSamples.map { sample_id =>
          {
            val sampleRsemDta = _rsemData.get((sample_id, gene.gene_id))

            val sampleTranscriptData1 = gene.transcripts
              .map { transcript => _trancriptData.get((sample_id, transcript.transcript_id)) }
              .filter { _.isDefined }
              .map { _.get }

            SampleDataOutput(sample_id, sampleRsemDta, if (sampleTranscriptData1.size > 0) Some(sampleTranscriptData1) else None)
          }
        }
        GeneLevelOutput(gene.gene_id, gene.gene_symbol, gene_data)

      }
    }

  }
}
