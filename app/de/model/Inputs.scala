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

/*object InputFilters {

  def writeJson(instance: InputFilters): JsValue = {
    val temp = instance.ref_id.get.writeJson.as[JsObject]
    val t1 = JsObjectWithOption(
      "study_id" -> Left(Json.toJson(instance.study_id)),
      "sample_id" -> Left(Json.toJson(instance.sample_id))).as[JsObject]
    t1.deepMerge(temp)
  }

  def getMongoQueryString(instance: InputFilters): String = {
    Seq(
      if (instance.study_id.size > 0) Some(s"""{study_id: {$$in: ${Inputs.seqAsMongoString(instance.study_id)}}}""") else None,
      if (instance.sample_id.size > 0) Some(s"""{sample_id: {$$in: ${Inputs.seqAsMongoString(instance.sample_id)}}}""") else None,
      instance.ref_id match {
        case Some(x) => x.queryMongoString
        case None    => None
      })
      .flatten
      .mkString("{ $and : [ ", ", ", " ] }")
  }

  def getCassandraQueryString(instance: InputFilters): String = {
    Seq(
      if (instance.study_id.size > 0) Some(s"{study_id in : ${instance.study_id.mkString("('", "','", "')")}") else None,
      if (instance.sample_id.size > 0) Some(s"{sample_id in : ${instance.sample_id.mkString("('", "','", "')")}") else None,
      instance.ref_id match {
        case Some(x) => x.queryCassandraString
        case None    => None
      })
      .flatten
      .mkString(" and ")
  }

  def getElasticSearchQueryString(instance: InputFilters): String = {

    Seq(
      if (instance.study_id.size > 0) Some(s"""{ "terms" : { "study_id" : ${instance.study_id.map { x => s""" "$x" """ }.mkString("[", ",", "]")} } }""") else None,
      if (instance.sample_id.size > 0) Some(s"""{ "terms" : { "sample_id" : ${instance.sample_id.map { x => s""" "$x" """ }.mkString("[", ",", "]")} } }""") else None,
      instance.ref_id match {
        case Some(x) => x.queryElasticSearchString
        case None    => None
      })
      .flatten
      .mkString(",")
  }

}*/
object Inputs {

  def stringWithDoubleQuotes(str: String) = s""""$str""""

  def seqAsMongoString(values: Seq[String]) = values
    .map {
      stringWithDoubleQuotes
    }
    .mkString("[", ", ", "]")

  trait FilterUnit {
    val key: String
    val values: Seq[String]
    def queryMongoString: Option[String] =
      if (values.size > 0)
        Some(s"""{$key: {$$in: ${seqAsMongoString(values)}}}""")
      else
        None
    def queryCassandraString: Option[String] =
      if (values.size > 0)
        Some(s"{$key in : ${values.mkString("('", "','", "')")}")
      else
        None
    def queryElasticSearchString: Option[String] =
      if (values.size > 0)
        Some(s"""{ "terms" : { "$key" : ${values.map { x => s""" "$x" """ }.mkString("[", ",", "]")} } }""")
      else
        None
  }

  case class StudyFilter(override val values: Seq[String]) extends FilterUnit {
    override val key = "study_id"
  }
  case class SampleFilter(override val values: Seq[String]) extends FilterUnit {
    override val key = "sample_id"
  }
  case class GeneIdFilter(override val values: Seq[String]) extends FilterUnit {
    override val key = "gene_id"
  }
  case class GeneSymbolFilter(override val values: Seq[String]) extends FilterUnit {
    override val key = "gene_symbol"
  }
  case class TranscriptIdFilter(override val values: Seq[String]) extends FilterUnit {
    override val key = "transcript_id"
  }

  trait GeneQueryRef {
    val ref_id: Seq[String]
  }

  case class GeneIdQuery(override val ref_id: Seq[String]) extends GeneQueryRef

  case class GeneSymbolQuery(override val ref_id: Seq[String]) extends GeneQueryRef

  case class TranscriptIdQuery(override val ref_id: Seq[String]) extends GeneQueryRef

  case class InputFilters(ref_id: Option[GeneQueryRef] = None,
                          study_id: Seq[String] = Seq(),
                          sample_id: Seq[String] = Seq())

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
