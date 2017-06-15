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

case class MongoRepositoryConfig(
  uri: String,
  databaseName: String)

final class MongoRepository(val conf: MongoRepositoryConfig) {

  private val client =
    new MongoClient(
      new MongoClientURI(
        conf.uri))

  private val dao: MongoDAO =
    new MongoDAO(
      client,
      conf.databaseName)

  def findData(collectionName: String, filters: Map[String, Seq[String]]) = {
    dao.findData(collectionName, filters)
  }

  def getDataByGeneSymbols(ref_ids: Seq[String], normalizations: Seq[ObjectFilters], targetSampleIds: Seq[String]) = {
    val genes = ref_ids
      .map { gene_symbol => GeneDataUtil.getGeneInputRef(GeneSymbolInput(gene_symbol)) }
      .filter(_.isDefined)
      .map(_.get)
    getData(genes, normalizations, targetSampleIds)
  }

  def getDataByGeneIds(ref_ids: Seq[String], normalizations: Seq[ObjectFilters], targetSampleIds: Seq[String]) = {
    val genes = ref_ids
      .map { gene_id => GeneDataUtil.getGeneInputRef(GeneIdInput(gene_id)) }
      .filter(_.isDefined)
      .map(_.get)
    getData(genes, normalizations, targetSampleIds)
  }

  def getDataByTranscriptIds(ref_ids: Seq[String], normalizations: Seq[ObjectFilters], targetSampleIds: Seq[String]) = {
    val genes = ref_ids
      .map { transcript_id => GeneDataUtil.getGeneInputRef(TranscriptIdInput(transcript_id)) }
      .filter(_.isDefined)
      .map(_.get).groupBy { _.gene_id }.mapValues(_.head).values.toSeq
    getData(genes, normalizations, targetSampleIds)
  }

  def getData(genes: Seq[GeneInfoOutput], data_types: Seq[ObjectFilters], study_ids: Seq[String]) = {
    val normalizations = data_types.groupBy { _.collection_name }.mapValues(_.head)
    val input_gene_ids = genes.map { _.gene_id }
    val input_transcript_ids = genes.flatMap { _.transcripts.map { _.transcript_id } }

    //START get abundance data
    val transcriptsAbundanceData = if (normalizations.get("transcript_abundance").isDefined) {
      val _normalization_input = normalizations("transcript_abundance").asInstanceOf[SampleAbundanceProjectons]
      val _filters = Map("transcript_id" -> input_transcript_ids, "study_id" -> study_ids)
      findData(_normalization_input.collection_name, _filters).map { _data =>
        {
          val transcript_id = (_data \ "transcript_id").as[String]
          _data.parseObjectArray("data").map { _obj => SampleAbundanceOutput.readJson(transcript_id = transcript_id, obj = _obj, projectionFileds = _normalization_input) }.map { _.get }
        }
      }.flatten.toSeq
    } else Seq[SampleAbundanceOutput]()

    val _abundanceData = transcriptsAbundanceData
      .map { sampleAbundance => (sampleAbundance.sample_id, sampleAbundance.transcript_id) -> sampleAbundance }.toMap
    //END get abundance data

    //START get isoform data
    val transcriptsIsoformData = if (normalizations.get("transcript_isoform").isDefined) {
      val _normalization_input = normalizations("transcript_isoform").asInstanceOf[SampleRsemIsoformProjectons]
      val _filters = Map("transcript_id" -> input_transcript_ids, "study_id" -> study_ids)
      findData(_normalization_input.collection_name, _filters).map { _data =>
        {
          val transcript_id = (_data \ "transcript_id").as[String]
          _data.parseObjectArray("data").map { _obj => SampleRsemIsoformOutput.readJson(transcript_id = transcript_id, obj = _obj, projectionFileds = _normalization_input) }.map { _.get }
        }
      }.flatten.toSeq
    } else Seq[SampleRsemIsoformOutput]()

    val _isoformData = transcriptsIsoformData
      .map { sampleRsemIsoform => (sampleRsemIsoform.sample_id, sampleRsemIsoform.transcript_id) -> sampleRsemIsoform }.toMap
    //END get isoform data

    //START get rsem gene data
    val geneRsemData = if (normalizations.get("gene_rsem").isDefined) {
      val _normalization_input = normalizations("gene_rsem").asInstanceOf[SampleRsemGeneProjectons]
      val _filters = Map("gene_id" -> input_gene_ids, "study_id" -> study_ids)
      findData(_normalization_input.collection_name, _filters).map { _data =>
        {
          val gene_id = (_data \ "gene_id").as[String]
          _data.parseObjectArray("data").map { _obj => SampleRsemGeneOutput.readJson(gene_id = gene_id, obj = _obj, projectionFileds = _normalization_input) }.map { _.get }
        }
      }.flatten.toSeq
    } else Seq[SampleRsemGeneOutput]()

    val _rsemData = geneRsemData.map { sampleRsemGene => (sampleRsemGene.gene_id, sampleRsemGene.sample_id) -> sampleRsemGene }.toMap
    //END get rsem gene data

    val _transcriptDataKeys = (_abundanceData.keySet ++ _isoformData.keySet).toSeq.distinct
    val _trancriptData1 = _transcriptDataKeys.map { case (sample_id, transcript_id) => (sample_id, transcript_id) -> TranscriptLevelOutput(transcript_id, _abundanceData.get((sample_id, transcript_id)), _isoformData.get((sample_id, transcript_id))) }.toMap

    val targetSamples = (_transcriptDataKeys.map { case (sample_id, _) => sample_id } ++ _rsemData.map { case ((_, sample_id), _) => sample_id }).toSeq.distinct
    genes.map { gene =>
      {
        val gene_data = targetSamples.map { sample_id =>
          {
            val _rsemData1 = _rsemData.get((gene.gene_id, sample_id))
            val _transcriptData1 = gene.transcripts.map { transcript => _trancriptData1.get((sample_id, transcript.transcript_id)) }.filter { _.isDefined }.map { _.get }
            SampleDataOutput(sample_id, _rsemData1, if (_transcriptData1.size > 0) Some(_transcriptData1) else None)
          }
        }
        GeneLevelOutput(gene.gene_id, gene.gene_symbol, gene_data)

      }
    }

  }
}