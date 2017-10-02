package de.service

import de.model.{ SampleAbundanceOutput, SampleDataOutput, SampleRsemGeneOutput, SampleRsemIsoformOutput, TranscriptLevelOutput }
import de.model.DomainTypes.{ GeneId, SampleId, StudyId, TranscriptId }
import de.model.GeneLevelOutput
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
                        filters: Seq[FilterUnit]): Iterable[SampleRsemGeneOutput]

    def getAbundanceData(projection: InputDataModel,
                         filters: Seq[FilterUnit]): Iterable[SampleAbundanceOutput]

    def getIsoformData(projection: InputDataModel,
                       filters: Seq[FilterUnit]): Iterable[SampleRsemIsoformOutput]

    def getData(filters: InputFilters,
                normalizations: Map[Normalization, InputDataModel]): Seq[GeneLevelOutput] = {

      //get queried gene objects
      val genes = filters.ref_id match {
        case Some(gene) => {
          GeneDataUtil.getGeneInputRef(gene)
        }
        case None => Seq()

      }

      val geneIdFilters = GeneIdFilter(genes.map { _.gene_id })
      val transcriptIdFilters = TranscriptIdFilter(genes.flatMap { _.transcripts.map { _.transcript_id } })
      val sampleFilters = SampleFilter(filters.sample_id)
      val studyFilters = StudyFilter(filters.study_id)

      val _abundanceData: Map[(SampleId, TranscriptId), SampleAbundanceOutput] =
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

      val _isoformData: Map[(SampleId, TranscriptId), SampleRsemIsoformOutput] =
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

      val _rsemData: Map[(SampleId, GeneId), SampleRsemGeneOutput] =
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

      val targetGeneIds = _rsemData.map { case ((_, gene_id), _) => gene_id } ++ _transcriptDataKeys.map {
        case (_, transcript_id) => {
          GeneDataUtil.getTranscript(transcript_id).map { x => x.gene_id }
        }
      }.flatten

      //join sample gene rsem data with transcript data
      //TODO: need to find a better way
      //case(gene_id, (gene_symbol, transcript_ids))
      // val outputGeneIds = _rsemData.map { case ((_, gene_id), _) => gene_id } ++ input_ids.map { case ((gene_id, gene_symbol), transcript_ids) => gene_id }
      targetGeneIds
        .toSeq
        .distinct
        .map { gene_id =>
          {
            val gene_info = GeneDataUtil.getGeneById(gene_id).get
            val gene_symbol = gene_info.gene_symbol
            val transcript_ids = gene_info.transcripts.map { _.transcript_id }

            val gene_data = targetSamples.map { sample_id =>
              {
                val sampleRsemData = _rsemData.get((sample_id, gene_id))

                val sampleTranscriptData = transcript_ids
                  .map { transcript_id => _trancriptData.get((sample_id, transcript_id)) }
                  .filter {
                    _.isDefined
                  }
                  .map {
                    _.get
                  }

                SampleDataOutput(sample_id, sampleRsemData, if (sampleTranscriptData
                  .nonEmpty) Some(sampleTranscriptData)
                else None)
              }
            }
            GeneLevelOutput(gene_id, gene_symbol, gene_data)

          }
        }
    }
  }
}
