package de.dao

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import com.sksamuel.elastic4s.IndexAndTypes.apply
import com.sksamuel.elastic4s.http.HttpClient
import com.sksamuel.elastic4s.http.search.SearchHit
import de.model.Inputs.FilterUnit
import de.repository.Dao

case class ElasticSearchSession(client: HttpClient)
class ElasticSearchDAO(session: ElasticSearchSession) extends Dao {
  override def find(collectionName: String,
                    filtersList: Seq[FilterUnit]): Iterable[SearchHit] = {

    import com.sksamuel.elastic4s.http.ElasticDsl._

    val jsonQuery = s"""{
        "constant_score" : {
            "filter" : {
                 "bool" : {
                    "must" : [
                        ${filtersList.flatMap { _.queryElasticSearchString }.mkString(",")}
                    ]
                  }
              }
          }
        }"""

    //some of the queries take lot of time
    implicit val duration: Duration = Duration.apply(2, TimeUnit.MINUTES)

    //TODO: elastic search type is hardcoded for now
    //and set size to 200000 i.e number of transcripts
    val query = search(collectionName / "default")
      .rawQuery(jsonQuery)
      .size(200000)

    val result = session.client.execute {
      query
    }.await

    new Iterable[SearchHit] {
      override def iterator() = { result.hits.hits.toIterator }
    }

  }
}
