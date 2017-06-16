package de.v2.model

package object DomainTypes {
  type GeneId = String
  type GeneSymbol = String
  type TranscriptId = String
  type SampleId = String
  type StudyId = String
  type Key = String
  type Value = String
}

object Inputs {

  trait GeneQueryRef

  case class GeneIdQuery(ref_id: String) extends GeneQueryRef
  case class GeneSymbolQuery(ref_id: String) extends GeneQueryRef
  case class TranscriptIdQuery(ref_id: String) extends GeneQueryRef

  trait ObjectFilters {
    val collection_name: String
  }

  case class Gene(
    chr: String,
    strand: String,
    gene_symbol: String,
    gene_id: String,
    gene_start: Long,
    gene_end: Long,
    gene_biotype: String,
    transcript_id: String,
    transcript_start: Long,
    transcript_end: Long,
    transcript_biotype: String,
    entrez_id: Seq[String],
    refseq_mrna_id: Seq[String],
    refseq_protein_id: Seq[String])

  object Gene {

    def apply(line: String): Gene = {
      val spl = line.split("\t", 14)
      Gene(spl(0),
        spl(1),
        spl(2),
        spl(3),
        spl(4).toLong,
        spl(5).toLong,
        spl(6),
        spl(7),
        spl(8).toLong,
        spl(9).toLong,
        spl(10),
        toSeq(spl(11)),
        toSeq(spl(12)),
        toSeq(spl(13)))
    }

    //convert empty value as empty list
    def toSeq(listAsString: String): Seq[String] = {
      val x = listAsString.split(",", -1)
        .map { _.trim() }
        .toSeq
      if (x == Seq("")) Seq() else x
    }

  }

  trait TranscriptModel extends ObjectFilters
  trait GeneModel extends ObjectFilters

  case class SampleAbundanceProjectons(
      length: Boolean = false,
      effective_length: Boolean = false,
      expected_count: Boolean = false,
      tpm: Boolean = true) extends TranscriptModel {
    val sample_id: Boolean = true
    val collection_name: String = "transcript_abundance"
  }

  // ===========================================================================
  case class SampleRsemGeneProjectons(
      length: Boolean = false,
      effective_length: Boolean = false,
      expected_count: Boolean = false,
      tpm: Boolean = false,
      fpkm: Boolean = true) extends TranscriptModel {
    val sample_id: Boolean = true
    val collection_name: String = "gene_rsem"
  }

  // ===========================================================================
  case class SampleRsemIsoformProjectons(
      length: Boolean = false,
      effective_length: Boolean = false,
      expected_count: Boolean = false,
      tpm: Boolean = true,
      fpkm: Boolean = false,
      isoform_percentage: Boolean = false) extends GeneModel {
    val sample_id: Boolean = true
    val collection_name: String = "transcript_isoform"
  }
}
