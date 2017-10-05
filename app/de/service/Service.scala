package de.service

import de.model.{ RsemGene, RsemIsoform, SampleData, TranscriptData }
import de.model.Abundance
import de.model.DomainTypes.{ GeneId, SampleId, TranscriptId }
import de.model.GeneData
import de.model.Inputs.{ FilterUnit, GeneIdFilter, InputDataModel, InputFilters, SampleFilter, StudyFilter, TranscriptIdFilter }
import de.utils.Enums.Normalization
import de.utils.GeneDataUtil
import io.swagger.annotations.ApiModel

trait ServiceComponent {
  def service: Service

  trait Service {

    def find(collectionName: String,
             filters: Seq[FilterUnit]): Any

    def getRsemGeneData(projection: InputDataModel,
                        filters: Seq[FilterUnit]): Iterable[RsemGene]

    def getAbundanceData(projection: InputDataModel,
                         filters: Seq[FilterUnit]): Iterable[Abundance]

    def getIsoformData(projection: InputDataModel,
                       filters: Seq[FilterUnit]): Iterable[RsemIsoform]

    def getData(filters: InputFilters,
                normalizations: Map[Normalization, InputDataModel]): Seq[GeneData] = {

      //get queried gene objects
      val genes = filters.ref_id match {
        case Some(gene) => {
          Some(GeneDataUtil.getGeneInputRef(gene))
        }
        case None => None

      }

      val tempGeneIdFilters  = genes match {
        case Some(x) => Some(x.map { _.gene_id })
        case None => None
      }

      //val tempGeneIdFilters = if (genes.size > 0) Some(genes.map { _.gene_id }) else Some(Seq())
      val geneIdFilters = GeneIdFilter(tempGeneIdFilters)

       val temptranscriptIdFilters = genes match {
        case Some(x) => Some(x.flatMap { _.transcripts.map { _.transcript_id } })
        case None => None
      }

     // val temptranscriptIdFilters = if (genes.size > 0) Some(genes.flatMap { _.transcripts.map { _.transcript_id } }) else Some(Seq())
      val transcriptIdFilters = TranscriptIdFilter(temptranscriptIdFilters)

      val sampleFilters = SampleFilter(filters.sample_id)

      val studyFilters = StudyFilter(filters.study_id)

      val _abundanceData: Map[(SampleId, TranscriptId), Abundance] =
        normalizations
          .get(Normalization.sample_abundance) match {
            case Some(projection) => getAbundanceData(
              projection,
              Seq(transcriptIdFilters, sampleFilters, studyFilters))
            .map { sampleAbundance =>
              (sampleAbundance.sample_id, sampleAbundance
                .transcript_id) -> sampleAbundance
            }
            .toMap
            case None => Map() // map it to empty Map if normalization is not passed in input query
          }

      val _isoformData: Map[(SampleId, TranscriptId), RsemIsoform] =
        normalizations
          .get(Normalization.sample_rsem_isoform) match {
            case Some(projection) => getIsoformData(
              projection,
              Seq(transcriptIdFilters, sampleFilters, studyFilters))
            .map { sampleRsemIsoform =>
              (sampleRsemIsoform.sample_id, sampleRsemIsoform
                .transcript_id) -> sampleRsemIsoform
            }
            .toMap
            case None => Map() // map it to empty Map if normalization is not passed in input query
          }

      val _rsemData: Map[(SampleId, GeneId), RsemGene] =
        normalizations
          .get(Normalization.rsem) match {
            case Some(projection) => getRsemGeneData(
              projection,
              Seq(geneIdFilters, sampleFilters, studyFilters))
            .map { sampleRsemGene =>
              (sampleRsemGene.sample_id, sampleRsemGene
                .gene_id) -> sampleRsemGene
            }
            .toMap
            case None => Map() // map it to empty Map if normalization is not passed in input query
          }

      //join sample transcript data by transcript ids
      val _transcriptDataKeys = (_abundanceData.keySet ++ _isoformData.keySet)
        .toSeq
        .distinct

      val _trancriptData = _transcriptDataKeys
        .map {
          case (sample_id, transcript_id) => (sample_id, transcript_id) ->
            TranscriptData(transcript_id,
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

      val targetGeneIds = _rsemData.map { case ((_, gene_id), _) => gene_id } ++ _transcriptDataKeys.flatMap {
        case (_, transcript_id) => {
          GeneDataUtil.getTranscript(transcript_id)
        }
      }.map { _.gene_id }

      //join sample gene rsem data with transcript data
      //TODO: need to find a better way
      targetGeneIds
        .toSeq
        .distinct
        .flatMap(GeneDataUtil.getGeneById)
        .map { gene_info =>
          {
            val gene_symbol = gene_info.gene_symbol
            val transcript_ids = gene_info.transcripts.map { _.transcript_id }

            val gene_data = targetSamples.map { sample_id =>
              {
                val sampleRsemData = _rsemData.get((sample_id, gene_info.gene_id))

                val sampleTranscriptData = transcript_ids
                  .flatMap { transcript_id => _trancriptData.get((sample_id, transcript_id)) }

                SampleData(sample_id, sampleRsemData, if (sampleTranscriptData
                  .nonEmpty) Some(sampleTranscriptData)
                else None)
              }
            }
            GeneData(gene_info.gene_id, gene_symbol, gene_data)

          }
        }
    }
  }
}
