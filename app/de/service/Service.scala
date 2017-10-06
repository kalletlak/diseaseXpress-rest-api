package de.service

import de.model.output.{ Abundance, GeneData, GeneInfo, RsemGene, RsemIsoform, SampleData, TranscriptData }
import de.model.DomainTypes.{ GeneId, SampleId, TranscriptId }
import de.model.Inputs.{ FilterUnit, GeneIdFilter, InputDataModel, InputFilters, SampleFilter, StudyFilter, TranscriptIdFilter }
import de.utils.Enums.Normalization
import de.utils.GeneDataUtil
import io.swagger.annotations.ApiModel
import utils.Implicits.AnythingImplicits

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
        filters:        InputFilters,
        normalizations: Map[Normalization, InputDataModel])
      : Seq[GeneData] = {

    // get queried gene objects
    val genes: Option[Seq[GeneInfo]] =
      filters
        .ref_id
        .map(GeneDataUtil.getGeneInputRef)

    val geneIdFilters: GeneIdFilter =
      genes
        .map(_.map(_.gene_id))
        .zen(GeneIdFilter) //val tempGeneIdFilters = if (genes.size > 0) Some(genes.map { _.gene_id }) else Some(Seq())
 
    val transcriptIdFilters: TranscriptIdFilter = 
     genes
       .map(
         _.flatMap(_.transcripts
           .map(_.transcript_id)))
       .zen(TranscriptIdFilter) // val temptranscriptIdFilters = if (genes.size > 0) Some(genes.flatMap { _.transcripts.map { _.transcript_id } }) else Some(Seq())    

    val sampleFilters =
      SampleFilter(filters.sample_id)

    val studyFilters =
      StudyFilter(filters.study_id)

    // ---------------------------------------------------------------------------
    // map them to empty Map if normalization is not passed in input query     
    
    val abundanceData: Map[(SampleId, TranscriptId), Abundance] =
      normalizations
        .get(Normalization.sample_abundance)
        .map(projection => 
          getAbundanceData(
              projection,
              filters = Seq(transcriptIdFilters, sampleFilters, studyFilters))
            .map { sampleAbundance =>
              (sampleAbundance.sample_id, sampleAbundance.transcript_id) ->
                sampleAbundance }
            .toMap)
        .getOrElse(Map()) 

    val isoformData: Map[(SampleId, TranscriptId), RsemIsoform] =
      normalizations
        .get(Normalization.sample_rsem_isoform)
        .map(projection => 
          getIsoformData(
            projection,
            Seq(transcriptIdFilters, sampleFilters, studyFilters))
          .map { sampleRsemIsoform =>
            (sampleRsemIsoform.sample_id, sampleRsemIsoform.transcript_id) ->
              sampleRsemIsoform }
          .toMap)
        .getOrElse(Map()) 

    val rsemData: Map[(SampleId, GeneId), RsemGene] =
      normalizations
        .get(Normalization.rsem)
        .map(projection => 
          getRsemGeneData(
            projection,
            Seq(geneIdFilters, sampleFilters, studyFilters))
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
