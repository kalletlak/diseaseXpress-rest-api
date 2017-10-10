package de.validators

import de.model.tags.Enums.study
import de.model.output.GeneInfo
import de.model.Error
import de.repository.GeneRepository

// ===========================================================================
sealed trait IdRef {
  val ref_id: Seq[String]
}

sealed trait SecondaryIdRef    extends IdRef
sealed trait PrimaryIdRef      extends IdRef

sealed trait GeneQuery       extends PrimaryIdRef
sealed trait TranscriptQuery extends PrimaryIdRef


sealed trait PrimaryIdsValidator {
  def apply(ids: String): Either[Error, Seq[GeneInfo]]
}

sealed trait SecondaryIdsValidator{
  def apply(ids: String): Either[Error, SecondaryIdRef]
}

case class GeneIdQuery      (override val ref_id: Seq[String]) extends GeneQuery
case class GeneSymbolQuery  (override val ref_id: Seq[String]) extends GeneQuery
case class TranscriptIdQuery(override val ref_id: Seq[String]) extends TranscriptQuery

case class StudyQuery       (override val ref_id: Seq[String]) extends SecondaryIdRef
case class SampleQuery      (override val ref_id: Seq[String]) extends SecondaryIdRef

// ===========================================================================

object GeneIdQuery extends PrimaryIdsValidator {
  override def apply(ids: String): Either[Error, Seq[GeneInfo]] = {
    val gene_ids = ids.split(",", -1)
    val invalid_gene_ids = gene_ids
      .filterNot { GeneRepository.isGeneIdPresent }
    if (invalid_gene_ids.isEmpty)
      Right(GeneRepository.getGeneInputRef(GeneIdQuery(gene_ids)))
    else
      Left(Error("gene_ids", invalid_gene_ids))
  }
}

// ===========================================================================

object GeneSymbolQuery extends PrimaryIdsValidator {
  override def apply(ids: String): Either[Error, Seq[GeneInfo]] = {
    val gene_symbols = ids.split(",", -1)
    val invalid_gene_symbols = gene_symbols
      .filterNot { GeneRepository.isGeneSymbolPresent }
    if (invalid_gene_symbols
      .isEmpty)
      Right(GeneRepository.getGeneInputRef(GeneSymbolQuery(gene_symbols)))
    else
      Left(Error("gene_symbols", invalid_gene_symbols))
  }
}

// ===========================================================================

object TranscriptIdQuery extends PrimaryIdsValidator {
  override def apply(ids: String): Either[Error, Seq[GeneInfo]] = {
    val transcript_ids = ids.split(",", -1)
    val invalid_transcript_ids = transcript_ids
      .filterNot { GeneRepository.isTranscriptIdPresent }
    if (invalid_transcript_ids
      .isEmpty)
      Right(GeneRepository.getGeneInputRef(TranscriptIdQuery(transcript_ids)))
    else
      Left(Error("transcript_ids", invalid_transcript_ids))
  }
}

// ===========================================================================

object StudyQuery extends SecondaryIdsValidator {

  override def apply(studies: String): Either[Error, SecondaryIdRef] = {

    val study_ids = studies.split(",", -1)
    val invalid_study_ids = study_ids
      .filterNot { study_id => study.withNameOption(study_id).isDefined }
    if (invalid_study_ids
      .isEmpty)
      Right(StudyQuery(study_ids))
    else
      Left(Error("study_ids", invalid_study_ids))

  }
}

// ===========================================================================

//TODO: update sample id validator
object SampleQuery extends SecondaryIdsValidator {

  override def apply(samples: String): Either[Error, SecondaryIdRef] = {
    Right(SampleQuery(samples.split(",", -1)))
  }
}
