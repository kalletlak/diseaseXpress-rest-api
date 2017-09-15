package de.v2

import com.datastax.driver.core.Session
import com.datastax.driver.core.ResultSet
import com.datastax.driver.core.querybuilder.QueryBuilder
import scala.collection.JavaConverters._

class CassandraDAO(session: Session) extends Dao {
  override def find(collectionName: String,
                    filters: Map[String, Seq[String]]): ResultSet = {

    val querystr = filters.map {
      case (identifier, ids) => {
        val ids_str = ids.mkString("('", "','", "')")
        s"$identifier in $ids_str"
      }
    }.mkString(s"select * from $collectionName where ", " and ", " ALLOW FILTERING")
    val start = System.currentTimeMillis()
    val result: ResultSet = session.execute(querystr)
    val executionTime = System.currentTimeMillis() - start
    println(s"""Execution Time to get data(cassandra) \n\t query : $querystr \n\t Time : $executionTime""")
    result

  }
}
