package de.v2.controllers

import java.net.InetAddress

import scala.{ Left, Right }
import scala.annotation.migration
import scala.collection.JavaConverters.seqAsJavaListConverter

import com.datastax.driver.core.Cluster
import com.mongodb.{ MongoClient, MongoClientURI }

import de.v2.{ CassandraRepository, CassandraService, MongoRepository, MongoService, ServiceComponent }
import de.v2.model.GeneLevelOutput
import de.v2.model.Inputs.{ SampleAbundanceProjectons, SampleRsemGeneProjectons, SampleRsemIsoformProjectons }
import de.v2.utils.{ JsObjectWithOption, LoggingAction, SampleDataUtil }
import de.v2.utils.Enums.{ IdQuery, Normalization, Projection }
import io.swagger.annotations.{ Api, ApiImplicitParams, ApiModel, ApiOperation, ApiParam }
import javax.inject.{ Inject, Singleton }
import play.api.http.HttpFilters
import play.api.libs.json.Json
import play.api.mvc.{ Accepting, Controller, RequestHeader }
import play.filters.gzip.GzipFilter
import io.swagger.annotations.{ ApiImplicitParam, Example, ExampleProperty }

class Filters @Inject() (gzipFilter: GzipFilter) extends HttpFilters {
  def filters = Seq(gzipFilter)
}
@Singleton // this is not necessary, I put it here so you know this is possible
class Context @Inject() (configuration: play.api.Configuration) {

  // Since this controller is not annotated with @Inject
  // it WILL NOT be used when binding components
  //  def this(configuration: play.api.Configuration) = this(configuration)

  private val AcceptsTsv = Accepting("text/tab-separated-values")

  private val mongo_uri = configuration.getString("disease-express.database.mongo.uri").get
  private val mongo_database = configuration.getString("disease-express.database.mongo.db").get
  private val cassandra_ips = configuration.getStringSeq("disease-express.database.cassandra.contact_points").get

  private val cassandra_database = configuration.getString("disease-express.database.cassandra.db").get
  private val cassandra_port = configuration.getInt("disease-express.database.cassandra.port").get

  val cluster = Cluster.builder()
    .addContactPoints(cassandra_ips.map { InetAddress.getByName(_) }.asJava).withPort(cassandra_port)
    .build()
  cluster.getConfiguration.getSocketOptions
  //.setConnectTimeoutMillis(50000)
  // .setReadTimeoutMillis(120000)
  val session = cluster.connect(cassandra_database)
  val cassandraService = new CassandraService with CassandraRepository {
    val context = session
  }

  val client: MongoClient =
    new MongoClient(new MongoClientURI(mongo_uri))

  val jongo =
    new org.jongo.Jongo(
      client
        // warning: see https://github.com/bguerout/jongo/issues/254
        .getDB(mongo_database))

  private val mongoService = new MongoService with MongoRepository {
    val context = jongo
  }

  def getService(): ServiceComponent = {
    return cassandraService
  }
}

@Api(value = "/Data",
  description = "Operations with Genes and Transcripts",
  produces = "application/json, text/tab-separated-values")
class GenomicData @javax.inject.Inject() (
  configuration: play.api.Configuration, context: Context)
    extends Controller {

  private val AcceptsTsv = Accepting("text/tab-separated-values")

  private val mongo_uri = configuration.getString("disease-express.database.mongo.uri").get
  private val mongo_database = configuration.getString("disease-express.database.mongo.db").get
  private val contact_points = configuration.getStringSeq("disease-express.database.cassandra.contact_points").get

  private val cassandra_database = configuration.getString("disease-express.database.cassandra.db").get
  private val cassandra_port = configuration.getInt("disease-express.database.cassandra.port").get

  private val repo = context.getService.service

  /**
   * Converts gene data object to tsv format
   */
  private def tsvFormat(norms: Seq[Normalization], result: Seq[GeneLevelOutput],
                        projection: Projection) = {
    val rsem = norms.contains(Normalization.rsem)
    val sample_abundance = norms.contains(Normalization.sample_abundance)
    val sample_rsem_isoform = norms.contains(Normalization.sample_rsem_isoform)
    val header = Seq(GeneLevelOutput.getHeader(rsem, sample_abundance,
      sample_rsem_isoform, norms,
      projection).mkString("\t"))
    val data = result
      .map { x =>
        GeneLevelOutput.getValues(x, norms, projection)
          .map { _.mkString("\t") }
          .mkString("\n")
      }
    (header ++ data).mkString("\n")
  }

  /**
   *
   *
   */
  private def getFields(projection: Projection,
                        normalizations: Seq[Normalization]) =
    (projection match {
      case Projection.detailed => normalizations.map { x =>
        (x, x match {
          case Normalization.rsem => SampleRsemGeneProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true,
            fpkm = true)
          case Normalization.sample_abundance => SampleAbundanceProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true)
          case Normalization.sample_rsem_isoform => SampleRsemIsoformProjectons(
            length = true,
            effective_length = true,
            expected_count = true,
            tpm = true,
            fpkm = true,
            isoform_percentage = true)
        })
      }
      case Projection.summary => normalizations.map { x =>
        (x,
          x match {
            case Normalization.rsem => SampleRsemGeneProjectons()
            case Normalization
              .sample_abundance => SampleAbundanceProjectons()
            case Normalization
              .sample_rsem_isoform => SampleRsemIsoformProjectons()
          })
      }
    })
      .toMap

  private def getData(query_ref: IdQuery,
                      _ids: String,
                      ref_ids: Either[Seq[String], Seq[String]],
                      normalizations: Option[String],
                      projection: Option[String])(implicit request: RequestHeader) = {

    //map projection to enum
    val projection_enum = projection match {
      case Some(projection) => Projection.withNameOption(projection)
      case None             => Some(Projection.summary)
    }

    //map normalizarions to enum
    val normalization_enums = normalizations match {
      case Some(x) => x.split(",")
        .map(normalization => normalization -> Normalization
          .withNameOption(normalization)).toMap
      case None => Map(
        Normalization.rsem.entryName -> Some(Normalization
          .rsem),
        Normalization.sample_abundance
          .entryName -> Some(Normalization.sample_abundance),
        Normalization.sample_rsem_isoform
          .entryName -> Some(Normalization
            .sample_rsem_isoform))
    }

    val norm_invalid = normalization_enums.filter { case (_, norm_enum) => !norm_enum.isDefined }

    val proj_invalid = !projection_enum.isDefined

    //TODO: need to find a better solution
    if (norm_invalid.size > 0 || proj_invalid) {
      val t1 = if (norm_invalid.size > 0)
        Some(norm_invalid
          .keySet
          .mkString("", ",",
            " , are invalid. Must be in [rsem, sample_abundance, sample_rsem_isoform]"))
      else None

      val t2 = if (proj_invalid)
        Some(
          s"""${
            projection.get
          }, is invalid. Must be summary or detailed """)
      else None

      BadRequest(JsObjectWithOption(
        "projection" -> Right(t2.map(x => Json
          .toJson(x))),
        "normalizations" -> Right(t1.map(x => Json
          .toJson(x)))))

    } else {

      val _projection_enum = projection_enum.get

      val _normalizations_enums = normalization_enums.values
        .map { _.get }
        .toSeq

      val start = System.currentTimeMillis()

      val _result = context.getService match {
        case x: CassandraService => {
          val sample_ids = ref_ids match {
            case Left(x)  => SampleDataUtil.getSamples(x)
            case Right(x) => x
          }
          repo.getData(query_ref,
            _ids.split(",")
              .toSeq
              .distinct,
            getFields(_projection_enum,
              _normalizations_enums),
            sample_ids)
        }
        case x: MongoService => {
          val study_ids = ref_ids match {
            case Left(x)  => x
            case Right(x) => Seq()
          }
          repo.getData(query_ref,
            _ids.split(",")
              .toSeq
              .distinct,
            getFields(_projection_enum,
              _normalizations_enums),
            study_ids)
        }
      }

      println("Total Execution : " + (System.currentTimeMillis() - start))

      render {
        case Accepts.Json() => Ok(Json.toJson(_result))
        case AcceptsTsv() => Ok(tsvFormat(_normalizations_enums, _result,
          _projection_enum))
      }

    }
  }

  //START : data by gene Id
  @ApiOperation(value = "get all expression data for given gene entrez ids",
    notes = "Returns expression data for given gene entrez ids",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIds(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {

    implicit request =>

      getData(IdQuery.GeneIdQuery,
        gene_ids,
        Left(Seq()),
        None,
        projection)

  }

  @ApiOperation(value = "get expression data for given gene entrez ids and studies",
    notes = "Returns expression data for given gene entrez ids and studies",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndStudies(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.GeneIdQuery,
        gene_ids,
        Left(studies_ids.split(",", -1)),
        None,
        projection)
  }

  @ApiOperation(value = "get expression data for given gene entrez ids",
    notes = "Returns expression data for given gene entrez ids",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      defaultValue = "rsem",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.GeneIdQuery,
        gene_ids,
        Left(Seq()),
        Some(normalizations),
        projection)
  }

  @ApiOperation(value = "get expression data for given gene entrez ids and studies",
    notes = "Returns expression data for given gene entrez ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneIdsAndStudiesAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene entrez ids. e.g. ENSG00000136997.14,ENSG00000000003.14") gene_ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.GeneIdQuery,
        gene_ids,
        Left(studies_ids.split(",", -1)),
        Some(normalizations),
        projection)

  }

  //END : data by gene id
  // START : data by gene symbol
  @ApiOperation(value = "get expression data for given gene symbols",
    notes = "Returns expression data for given gene symbols",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbols(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") gene_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.GeneSymbolQuery,
        gene_ids,
        Left(Seq()),
        None,
        projection)

  }

  @ApiOperation(value = "get expression data for given gene symbols and studies",
    notes = "Returns expression data for given gene symbols and studies",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndStudies(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") _ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.GeneSymbolQuery,
        _ids,
        Left(studies_ids.split(",", -1)),
        None,
        projection)
  }

  @ApiOperation(value = "get expression data for given gene symbols",
    notes = "Returns expression data for given gene symbols",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") _ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.GeneSymbolQuery,
        _ids,
        Left(Seq()),
        Some(normalizations),
        projection)
  }

  @ApiOperation(value = "get expression data for given gene symbols and studies",
    notes = "Returns expression data for given gene symbols and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByGeneSymbolsAndStudiesAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") _ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.GeneSymbolQuery,
        _ids,
        Left(studies_ids.split(",", -1)),
        Some(normalizations),
        projection)

  }

  //END : data by gene symbol
  // START : data by transcript id
  @ApiOperation(value = "get expression data for given transcript ids",
    notes = "Returns expression data for given transcript ids",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIds(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") gene_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.TranscriptIdQuery,
        gene_ids,
        Left(Seq()),
        None,
        projection)

  }

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[GeneLevelOutput],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndStudies(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") _ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>
      getData(IdQuery.TranscriptIdQuery,
        _ids,
        Left(studies_ids.split(",", -1)),
        None,
        projection)
  }

  @ApiOperation(value = "get expression data for given transcript ids",
    notes = "Returns expression data for given transcript ids",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") _ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>

      getData(IdQuery.TranscriptIdQuery,
        _ids,
        Left(Seq()),
        Some(normalizations),
        projection)
  }

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET")
  def getDataByTranscriptIdsAndStudiesAndNormalizations(
    @ApiParam(
      value = "Comma separated list of transcript ids. e.g. ENST00000373031.4,ENST00000514373.2") _ids: String,
    @ApiParam(
      value = "Comma separated list of study ids. e.g. PNOC,TARGET") studies_ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction {
    implicit request =>

      getData(IdQuery.TranscriptIdQuery,
        _ids,
        Left(studies_ids.split(",", -1)),
        Some(normalizations),
        projection)

  }

  //END : data by transcript id

  @ApiOperation(value = "get expression data for given transcript ids and studies",
    notes = "Returns expression data for given transcript ids and studies",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "payload",
      required = true,
      value = """Its a JSON payload currently supports the following operations
                     &ensp; Logical Operators : $and, $not, $nor, $or
                     &ensp; Comparison Operators : $eq, $gt, $gte, $in, $lt, $lte, $ne, $nin
                {
                 &ensp;&ensp;"$and": [
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$eq": { "mycn_status":"amplified" }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$in": { "risk": ["high","low"] }},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$not": { "$eq": { "stage": 4 }}},
                 &ensp;&ensp;&ensp;&ensp;&ensp;{ "$or": [
    	           &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "risk": "high" },
                 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;{ "stage": 4 }]
                 &ensp;&ensp;&ensp;&ensp;&ensp;}]
                } """,
      paramType = "body",
      examples = new Example(Array(new ExampleProperty(mediaType = "String", value = "\"test\""))) //examples isn't working with this swaggger version
      )))
  def getDataByGeneSymbolsAndTagsAndNormalizations(
    @ApiParam(
      value = "Comma separated list of gene symbols. e.g. MYCN,TP53") _ids: String,
    @ApiParam(
      value = "Comma separated list of normalization methods",
      allowableValues = "rsem,sample_abundance,sample_rsem_isoform",
      allowMultiple = true) normalizations: String,
    @ApiParam(
      value = "Projection type summary or detailed",
      allowableValues = "summary,detailed",
      defaultValue = "summary") projection: Option[String]) = LoggingAction(bodyParser = parse.json) {
    implicit request =>

      val json = request.body
      try {
        getData(IdQuery.GeneSymbolQuery,
          _ids,
          Right(SampleDataUtil.getSamples(json).toSeq),
          Some(normalizations),
          projection)
      } catch {
        case x: AssertionError => {
          println(x.printStackTrace())
          BadRequest("Got some other kind of exception AssertionError")
        }
        case x: Throwable => {
          println(x.printStackTrace())
          BadRequest("Got some other kind of exception")
        }
      }

  }

}
