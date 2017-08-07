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

  case class Gene(chr: String,
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
      val itr = line.split("\t", 14).iterator
      Gene(itr.next,
            itr.next,
            itr.next,
            itr.next,
            itr.next.toLong,
            itr.next.toLong,
            itr.next,
            itr.next,
            itr.next.toLong,
            itr.next.toLong,
            itr.next,
            toSeq(itr.next),
            toSeq(itr.next),
            toSeq(itr.next))
    }

    //convert empty value as empty list
    def toSeq(listAsString: String): Seq[String] = {
      listAsString match {
        case ""                   => Seq()
        case nonEmptyListAsString => nonEmptyListAsString.split(",", -1)
          .map(_.trim).toSeq
      }
    }

  }

  trait InputDataModel {
    val collection_name: String
  }

  trait TranscriptModel extends InputDataModel

  trait GeneModel extends InputDataModel

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
