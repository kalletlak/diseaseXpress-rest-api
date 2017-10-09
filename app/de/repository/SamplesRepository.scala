package de.repository

import de.dao.SamplesDAO
import de.model.tags.Sample
import play.api.libs.json.JsValue
import de.utils.MongoQuery
import de.utils.Query

object SamplesRepository {
  private val dao = SamplesDAO
  
  def getStudies:                                  Seq[String] = dao.getStudies
  
  def getSamplesIds(studies: Seq[String] = Nil)  : Seq[String] = dao.getSamplesInfo(studies)
                                                                  .map(_.sample_id.value)
                                                                  
  def getSamplesInfo (studies: Seq[String] = Nil): Seq[Sample] = dao.getSamplesInfo(studies)
  
  def getSamples(value: JsValue)                 : Seq[String] = 
    MongoQuery
          .getMongoQuery(value) match {
            case query: Query => dao.getSamples(query)
            case _            => Seq()
          }
}