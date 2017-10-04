package de

import java.net.InetAddress

import scala.collection.JavaConverters.seqAsJavaListConverter

import com.datastax.driver.core.Cluster
import com.mongodb.{ MongoClient, MongoClientURI }
import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.HttpClient

import de.dao.ElasticSearchSession
import de.repository.{ CassandraRepository, ElasticSearchRepository, MongoRepository }
import de.service.{ CassandraService, ElasticSearchService, MongoService, ServiceComponent }
import javax.inject.{ Inject, Singleton }
import play.api.mvc.Accepting

@Singleton // this is not necessary, I put it here so you know this is possible
class Context @Inject() (configuration: play.api.Configuration) {

  // Since this controller is not annotated with @Inject
  // it WILL NOT be used when binding components
  //  def this(configuration: play.api.Configuration) = this(configuration)

  //Elasticsearch service START
  private val elasticsearch_uri = configuration.getString("disease-express.database.elasticsearch.uri").get
  private val elasticsearch_database = configuration.getString("disease-express.database.elasticsearch.db").get

  private val elastic_client = HttpClient(ElasticsearchClientUri(elasticsearch_uri))

  private val elasticsearchService = new ElasticSearchService with ElasticSearchRepository {
    val context = new ElasticSearchSession(elastic_client)
  }
  //Elasticsearch service END

  //Cassandra service START
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
  //Cassandra service END

  //Mongodb service START
  private val mongo_uri = configuration.getString("disease-express.database.mongo.uri").get
  private val mongo_database = configuration.getString("disease-express.database.mongo.db").get

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
  //Mongodb service END

  def getService(): ServiceComponent = {
    return elasticsearchService
  }
}
