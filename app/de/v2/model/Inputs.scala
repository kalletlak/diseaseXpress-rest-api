package de.v2.model

object Inputs {

  case class GeneQuery(
    gene_id: String,
    gene_symbol: String,
    transcript_ids: Seq[String])

  trait GeneQueryRef

  case class GeneIdInput(ref_id: String) extends GeneQueryRef
  case class GeneSymbolInput(ref_id: String) extends GeneQueryRef
  case class TranscriptIdInput(ref_id: String) extends GeneQueryRef

  object GeneQuery {

    def apply(line: String): GeneQuery = {
      val spl = line.split("\t", -1).iterator
      GeneQuery(
        gene_id = spl.next,
        gene_symbol = spl.next,
        transcript_ids = spl.next
          .split(",")
          .map { _.trim })
    }

  }

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
      Gene(spl(0), spl(1), spl(2), spl(3), spl(4).toLong, spl(5).toLong, spl(6), spl(7), spl(8).toLong, spl(9).toLong, spl(10), toSeq(spl(11)), toSeq(spl(12)), toSeq(spl(13)))
    }

    def toSeq(listAsString: String): Seq[String] = { val x = listAsString.split(",", -1).map { _.trim() }.toSeq; if (x == Seq("")) Seq() else x }

  }

  case class SampleAbundanceProjectons(
      length: Boolean = false,
      effective_length: Boolean = false,
      expected_count: Boolean = false,
      tpm: Boolean = true) extends ObjectFilters {
    val sample_id: Boolean = true
    val collection_name: String = "transcript_abundance"
  }

  // ===========================================================================	
  case class SampleRsemGeneProjectons(
      length: Boolean = false,
      effective_length: Boolean = false,
      expected_count: Boolean = false,
      tpm: Boolean = false,
      fpkm: Boolean = true) extends ObjectFilters {
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
      isoform_percentage: Boolean = false) extends ObjectFilters {
    val sample_id: Boolean = true
    val collection_name: String = "transcript_isoform"
  }
}
