package de.v2

import org.jongo.Jongo
import com.datastax.driver.core.Session

trait Dao {
  def find(collectionName: String,
           filters: Map[String, Seq[String]]): Any
}

trait Repository {
  def dao: Dao
}

trait MongoRepository extends Repository {

  val context: Jongo
  def dao = {
    new MongoDAO(context)
  }
}

trait CassandraRepository extends Repository {

  val context: Session
  def dao = {
    new CassandraDAO(context)
  }

}
