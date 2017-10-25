package de.repository

import de.dao.SamplesDAO
import de.model.DomainTypes.{ SampleId, StudyId }
import de.model.tags.Sample
import de.utils.Queryparser.Query

object SamplesRepository {
  
  private val dao = SamplesDAO
  
  def getStudies:                                    Seq[String] = dao.getStudies
  
  def getSamplesIds(studies: Seq[StudyId] = Nil):    Seq[String] = dao.getSamplesInfo(studies)
                                                                  .map(_.sample_id.value)
                                                                  
  def getSamplesInfo (studies: Seq[SampleId] = Nil): Seq[Sample] = dao.getSamplesInfo(studies)
  
  def getSamples(query: Query):                      Seq[String] = dao.getSamples(query)
  
  def isStudyPresent(studyId:StudyId):               Boolean     = dao.isStudyPresent(studyId)
}