package de.service

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

import com.datastax.driver.core.ResultSet

import de.model.{ SampleAbundanceOutput, SampleRsemGeneOutput, SampleRsemIsoformOutput }
import de.model.Inputs.{ FilterUnit, InputDataModel }
import de.repository.Repository
import io.swagger.annotations.ApiModel

trait CassandraService extends ServiceComponent {
  this: Repository =>

  def service = new DefaultService

  class DefaultService extends Service {
    override def find(collectionName: String,
                      filters: Seq[FilterUnit]) = dao.find(collectionName, filters)

    def getRsemGeneData(projection: InputDataModel,
                        filters: Seq[FilterUnit]): Iterable[SampleRsemGeneOutput] = {

      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet].asScala.map { row =>
          SampleRsemGeneOutput.readRow(row,
            projection).get
        }
    }

    def getAbundanceData(projection: InputDataModel,
                         filters: Seq[FilterUnit]): Iterable[SampleAbundanceOutput] = {

      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet]
        .asScala
        .map { row =>
          SampleAbundanceOutput.readRow(row,
            projection).get
        }
    }

    def getIsoformData(projection: InputDataModel,
                       filters: Seq[FilterUnit]): Iterable[SampleRsemIsoformOutput] = {

      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet].asScala.map { row =>
          SampleRsemIsoformOutput.readRow(row,
            projection).get
        }

    }

  }
}
