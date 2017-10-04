package de.model

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

  def stringWithDoubleQuotes(str: String) = s""""$str""""

  def seqAsMongoString(values: Seq[String]) =
    values
      .map {
        stringWithDoubleQuotes
      }
      .mkString("[", ", ", "]")

  trait FilterUnit {
    val key: String
    val values: Option[Seq[String]]
    def queryMongoString: Option[String] =
      values match {
        case Some(_values) => Some(s"""{$key: {$$in: ${seqAsMongoString(_values)}}}""")
        case None          => None
      }

    def queryCassandraString: Option[String] =
      values match {
        case Some(_values) => Some(s"$key in ${_values.mkString("('", "','", "')")}")
        case None          => None
      }

    def queryElasticSearchString: Option[String] =
      values match {
        case Some(_values) => Some(s"""{ "terms" : { "$key" : ${_values.map { x => s""" "$x" """ }.mkString("[", ",", "]")} } }""")
        case None          => None
      }
  }

  case class StudyFilter(override val values: Option[Seq[String]]) extends FilterUnit {
    override val key = "study_id"
  }
  case class SampleFilter(override val values: Option[Seq[String]]) extends FilterUnit {
    override val key = "sample_id"
  }
  case class GeneIdFilter(override val values: Option[Seq[String]]) extends FilterUnit {
    override val key = "gene_id"
  }
  case class GeneSymbolFilter(override val values: Option[Seq[String]]) extends FilterUnit {
    override val key = "gene_symbol"
  }
  case class TranscriptIdFilter(override val values: Option[Seq[String]]) extends FilterUnit {
    override val key = "transcript_id"
  }

  trait GeneQueryRef {
    val ref_id: Seq[String]
  }

  case class GeneIdQuery(override val ref_id: Seq[String]) extends GeneQueryRef

  case class GeneSymbolQuery(override val ref_id: Seq[String]) extends GeneQueryRef

  case class TranscriptIdQuery(override val ref_id: Seq[String]) extends GeneQueryRef

  case class InputFilters(ref_id: Option[GeneQueryRef] = None,
                          study_id: Option[Seq[String]] = None,
                          sample_id: Option[Seq[String]] = None)

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
        case "" => Seq()
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
