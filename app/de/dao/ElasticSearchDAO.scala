package de.dao

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import com.sksamuel.elastic4s.IndexAndTypes.apply
import com.sksamuel.elastic4s.http.HttpClient
import com.sksamuel.elastic4s.http.search.SearchHit
import com.sksamuel.elastic4s.http.search.SearchResponse
import de.model.input.FilterUnit
import de.repository.Dao
import play.api.libs.json.Json

// ===========================================================================
case class ElasticSearchSession(
    client:      HttpClient, 
    size:        Int       = 200000, 
    timeout_min: Int       = 5)

// ===========================================================================
class ElasticSearchDAO(session: ElasticSearchSession) extends Dao {
  
  import com.sksamuel.elastic4s.http.ElasticDsl._
    
  override def find(
          collectionName: String,
          filtersList:    Seq[FilterUnit])
      : Iterable[SearchHit] = {

    val jsonQuery =
      Json.obj("constant_score" ->
        Json.obj("filter" ->
          Json.obj("bool" ->
            Json.obj("must" -> filtersList.map { _.queryElasticSearchString }))))
    
    //some of the queries take lot of time
    implicit val duration: Duration =
      Duration.apply(session.timeout_min, TimeUnit.MINUTES)

    val query =
      search(collectionName / "default")
        .rawQuery(jsonQuery.toString())
        .size(session.size)

    val result: SearchResponse =
      session
        .client
        .execute(query)
        .await      

    new Iterable[SearchHit] {
      override def iterator() = {
        result.hits.hits.toIterator }
    }

  }
  
}

// ===========================================================================