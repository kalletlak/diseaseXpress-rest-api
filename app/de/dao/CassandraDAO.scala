package de.dao

import com.datastax.driver.core.{ ResultSet, Session }
import de.model.Inputs.FilterUnit
import de.repository.Dao

class CassandraDAO(session: Session) extends Dao {
  override def find(collectionName: String,
                    filters: Seq[FilterUnit]): ResultSet = {

    val filters_str = filters
      .map { x => x.queryCassandraString }
      .mkString(" and ")

    val query_str = s"select * from $collectionName where $filters_str ALLOW FILTERING"

    session.execute(query_str)

  }
}
