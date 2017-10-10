package de.validators

import de.model.{ Error, GeneIdError, GeneSymbolError, StudyIdError, TranscriptIdError }
import de.model.output.GeneInfo
import de.repository.{ GeneRepository, SamplesRepository }
import io.swagger.annotations.ApiModel

// ===========================================================================
sealed trait IdRef {
  val ref_id: Seq[String]
}

sealed trait SecondaryIdRef extends IdRef
sealed trait PrimaryIdRef extends IdRef

sealed trait GeneQuery extends PrimaryIdRef
sealed trait TranscriptQuery extends PrimaryIdRef

sealed trait PrimaryIdsValidator {
  def apply(ids: Seq[String]): Either[Error, Seq[GeneInfo]]
}

sealed trait SecondaryIdsValidator {
  def apply(ids: Seq[String]): Either[Error, SecondaryIdRef]
}

case class GeneIdQuery(override val ref_id: Seq[String]) extends GeneQuery
case class GeneSymbolQuery(override val ref_id: Seq[String]) extends GeneQuery
case class TranscriptIdQuery(override val ref_id: Seq[String]) extends TranscriptQuery

case class StudyQuery(override val ref_id: Seq[String]) extends SecondaryIdRef
case class SampleQuery(override val ref_id: Seq[String]) extends SecondaryIdRef

// ===========================================================================

object GeneIdFilters extends PrimaryIdsValidator {
  override def apply(ids: Seq[String]): Either[Error, Seq[GeneInfo]] = {

    val invalid_gene_ids =
      ids
        .filterNot { GeneRepository.isGeneIdPresent }

    if (invalid_gene_ids.isEmpty)
      Right(GeneRepository.getGeneInputRef(GeneIdQuery(ids)))

    else
      Left(GeneIdError(invalid_gene_ids))
  }
}

// ===========================================================================

object GeneSymbolFilters extends PrimaryIdsValidator {

  override def apply(ids: Seq[String]): Either[Error, Seq[GeneInfo]] = {

    val invalid_gene_symbols =
      ids
        .filterNot { GeneRepository.isGeneSymbolPresent }

    if (invalid_gene_symbols.isEmpty)
      Right(GeneRepository.getGeneInputRef(GeneSymbolQuery(ids)))

    else
      Left(GeneSymbolError(invalid_gene_symbols))
  }
}

// ===========================================================================

object TranscriptIdFilters extends PrimaryIdsValidator {

  override def apply(ids: Seq[String]): Either[Error, Seq[GeneInfo]] = {

    val invalid_transcript_ids =
      ids
        .filterNot { GeneRepository.isTranscriptIdPresent }

    if (invalid_transcript_ids.isEmpty)
      Right(GeneRepository.getGeneInputRef(TranscriptIdQuery(ids)))

    else
      Left(TranscriptIdError(invalid_transcript_ids))
  }
}

// ===========================================================================

object StudyIdFilters extends SecondaryIdsValidator {

  override def apply(studies: Seq[String]): Either[Error, SecondaryIdRef] = {

    val invalid_study_ids =
      studies
        .filterNot { SamplesRepository.isStudyPresent }

    if (invalid_study_ids.isEmpty)
      Right(StudyQuery(studies))

    else
      Left(StudyIdError(invalid_study_ids))

  }
}

// ===========================================================================

//TODO: update sample id validator
//Currently sample ids are not passed as a request parameter
object SampleIdFilters extends SecondaryIdsValidator {

  override def apply(samples: Seq[String]): Either[Error, SecondaryIdRef] = {
    Right(SampleQuery(samples))
  }
}
