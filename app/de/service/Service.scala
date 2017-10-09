package de.service

import de.model.output.{ Abundance, GeneData, GeneInfo, RsemGene, RsemIsoform, SampleData, TranscriptData }
import de.model.input.{ FilterUnit, GeneIdFilter, InputDataModel, InputFilters, SampleFilter, StudyFilter, TranscriptIdFilter }
import de.utils.Enums.Normalization
import de.utils.GeneDataUtil
import io.swagger.annotations.ApiModel
import utils.Implicits.AnythingImplicits
import de.validators.StudyQuery
import de.validators.SampleQuery
import de.model.DomainTypes.SampleId
import de.model.DomainTypes.TranscriptId
import de.model.DomainTypes.GeneId

// ===========================================================================
trait ServiceComponent { def service: Service }

// ===========================================================================
trait Service {

  def getRsemGeneData(
      projection: InputDataModel,
      filters:    Seq[FilterUnit])
    : Iterable[RsemGene]

  // ---------------------------------------------------------------------------
  def getAbundanceData(
      projection: InputDataModel,
      filters:    Seq[FilterUnit])
    : Iterable[Abundance]

  // ---------------------------------------------------------------------------
  def getIsoformData(
      projection: InputDataModel,
      filters:    Seq[FilterUnit])
    : Iterable[RsemIsoform]

  // ===========================================================================
  protected def find(
      collectionName: String,
      filters:        Seq[FilterUnit])
    : Any // TODO: don't return Any

  // ===========================================================================
  final def getData(
        filters:        InputFilters)
      : Seq[GeneData] = {

    // get queried gene objects
    val genes: Seq[GeneInfo] =
      filters.primary_ref_ids

    val geneIdFilters: GeneIdFilter =
      genes
        .map(_.gene_id)
        .zen(GeneIdFilter) //val tempGeneIdFilters = if (genes.size > 0) Some(genes.map { _.gene_id }) else Some(Seq())

    val transcriptIdFilters:TranscriptIdFilter =
     genes
       .flatMap(_.transcripts
           .map(_.transcript_id))
       .zen(TranscriptIdFilter) // val temptranscriptIdFilters = if (genes.size > 0) Some(genes.flatMap { _.transcripts.map { _.transcript_id } }) else Some(Seq())

    val secondary_filters:FilterUnit = filters.secondary_ref_ids match {
      case SampleQuery(x) => SampleFilter(x)
      case StudyQuery(x)  => StudyFilter(x)
    }

    // ---------------------------------------------------------------------------
    // map them to empty Map if normalization is not passed in input query
    
    val abundanceData: Map[(SampleId, TranscriptId), Abundance] =
      filters.normalization_combo
        .get(Normalization.sample_abundance)
        .map(projection =>
          getAbundanceData(
              projection,
              filters = Seq(transcriptIdFilters, secondary_filters))
            .map { sampleAbundance =>
              (sampleAbundance.sample_id, sampleAbundance.transcript_id) ->
                sampleAbundance }
            .toMap)
        .getOrElse(Map())

    val isoformData: Map[(SampleId, TranscriptId), RsemIsoform] =
      filters.normalization_combo
        .get(Normalization.sample_rsem_isoform)
        .map(projection =>
          getIsoformData(
            projection,
            Seq(transcriptIdFilters, secondary_filters))
          .map { sampleRsemIsoform =>
            (sampleRsemIsoform.sample_id, sampleRsemIsoform.transcript_id) ->
              sampleRsemIsoform }
          .toMap)
        .getOrElse(Map())

    val rsemData: Map[(SampleId, GeneId), RsemGene] =
      filters.normalization_combo
        .get(Normalization.rsem)
        .map(projection =>
          getRsemGeneData(
            projection,
            Seq(geneIdFilters, secondary_filters))
          .map { sampleRsemGene =>
            (sampleRsemGene.sample_id, sampleRsemGene.gene_id) ->
              sampleRsemGene }
          .toMap)
        .getOrElse(Map())

    // ---------------------------------------------------------------------------
    // join sample transcript data by transcript ids
        
    val _transcriptDataKeys: Seq[(SampleId, TranscriptId)] =
      (abundanceData.keySet ++ isoformData.keySet)
        .toSeq
        .distinct

    val _trancriptData: Map[(SampleId, TranscriptId), TranscriptData] =
      _transcriptDataKeys
        .map { case (sample_id, transcript_id) =>
          (sample_id, transcript_id) ->
            TranscriptData(
               transcript_id       = transcript_id,
               sample_abundance    = abundanceData.get((sample_id, transcript_id)),
               sample_rsem_isoform = isoformData  .get((sample_id, transcript_id))) }
        .toMap
        
    // ---------------------------------------------------------------------------
    // these are the samples which should be same as input sample list but filtering out the samples which doesn't have data    
    val targetSamples: Seq[SampleId] =      
      (_transcriptDataKeys
        .map { case (sample_id, _) =>
          sample_id } ++
      rsemData
        .map { case ((sample_id, _), _) =>
            sample_id } )

        .toSeq
        .distinct

    // ---------------------------------------------------------------------------
    val geneIds: Iterable[GeneId] =
      ( rsemData
          .map { case ((_, gene_id), _) =>
            gene_id } ++ 
        _transcriptDataKeys
          .flatMap { case (_, transcript_id) =>
            GeneDataUtil.getTranscript(transcript_id) }
          .map { _.gene_id } )

    // ---------------------------------------------------------------------------
    //join sample gene rsem data with transcript data
    //TODO: need to find a better way
    geneIds
      .toSeq
      .distinct
      .flatMap(GeneDataUtil.getGeneById)
      .map(geneData(
        targetSamples,
        rsemData,
        _trancriptData))
  }
  
  // ===========================================================================
  private def geneData
        (targetSamples:  Seq[SampleId],
         _rsemData:      Map[(SampleId, GeneId), RsemGene],
         _trancriptData: Map[(SampleId, TranscriptId), TranscriptData])
        (gene_info: GeneInfo)
      : GeneData = {
    
    val transcript_ids: Seq[String] =
      gene_info
        .transcripts
        .map { _.transcript_id }

    val sampleData: Seq[SampleData] =
      targetSamples
        .map { sample_id =>
          val sampleTranscriptData: Seq[TranscriptData] =
            transcript_ids
              .flatMap { transcript_id =>
                _trancriptData.get(sample_id, transcript_id) }

          SampleData(
            sample_id,
            
            rsem =
              _rsemData
                .get(sample_id, gene_info.gene_id),

            transcripts =
              if (sampleTranscriptData.nonEmpty)
                Some(sampleTranscriptData)
              else
                None) }

    GeneData(
      gene_info.gene_id,
      gene_info.gene_symbol,
      sampleData)              
  }
  
}

// ===========================================================================
