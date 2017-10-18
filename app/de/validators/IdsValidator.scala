package de.validators

import de.model.{ Error, GeneIdError, GeneSymbolError, StudyIdError, TranscriptIdError }
import de.model.output.GeneInfo
import de.repository.{ GeneRepository, SamplesRepository }
import play.api.libs.json.JsValue
import de.utils.Queryparser

// ===========================================================================
sealed trait IdRef {
  val ref_id: Seq[String]
}

sealed trait PrimaryIdRef extends IdRef
sealed trait SecondaryIdRef extends IdRef
sealed trait GeneQuery extends PrimaryIdRef
sealed trait TranscriptQuery extends PrimaryIdRef

sealed trait PrimaryIdsValidator {
  def apply(ids: Seq[String]): Either[Error, Seq[GeneInfo]]
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

object SecondaryIds {
  def apply(obj: Either[Seq[String], JsValue]):Either[Error, SecondaryIdRef] = {
    obj match {
      case Left(studies) => {
        val invalid_study_ids =
          studies
            .filterNot { SamplesRepository.isStudyPresent }

        if (invalid_study_ids.isEmpty)
          Right(StudyQuery(studies))

        else
          Left(StudyIdError(invalid_study_ids))
      }

      case Right(tags) => {

        val query = Queryparser.getMongoQuery(tags)
        val result = query match {
          case Left(error)   => Left(error)
          case Right(result) => Right(SampleQuery(SamplesRepository.getSamples(query.right.get)))
        }
        result

      }
    }
  }
}
