package de.repository

import de.dao.SamplesDAO
import de.model.DomainTypes.{ SampleId, StudyId }
import de.model.tags.Sample
import de.utils.MongoQuery
import play.api.libs.json.JsValue

object SamplesRepository {
  
  private val dao = SamplesDAO
  
  def getStudies:                                    Seq[String] = dao.getStudies
  
  def getSamplesIds(studies: Seq[StudyId] = Nil):    Seq[String] = dao.getSamplesInfo(studies)
                                                                  .map(_.sample_id.value)
                                                                  
  def getSamplesInfo (studies: Seq[SampleId] = Nil): Seq[Sample] = dao.getSamplesInfo(studies)
  
  def getSamples(value: JsValue):                    Seq[String] = {
    val query = MongoQuery.getMongoQuery(value)
    dao.getSamples(query)
  }
  
  def isStudyPresent(studyId:StudyId):               Boolean     = dao.isStudyPresent(studyId)
}