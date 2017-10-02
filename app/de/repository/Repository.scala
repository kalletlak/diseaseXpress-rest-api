package de.repository

import org.jongo.Jongo

import com.datastax.driver.core.Session

import de.model.Inputs.FilterUnit
import de.dao.MongoDAO
import de.dao.ElasticSearchSession
import de.dao.ElasticSearchDAO
import de.dao.CassandraDAO

trait Dao {
  def find(collectionName: String,
             filters: Seq[FilterUnit]): Any
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

trait ElasticSearchRepository extends Repository {

  val context: ElasticSearchSession
  def dao = {
    new ElasticSearchDAO(context)
  }
}
