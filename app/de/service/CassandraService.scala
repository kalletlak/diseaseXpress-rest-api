package de.service

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

import com.datastax.driver.core.ResultSet

import de.model.{ RsemGene, RsemIsoform, Abundance }
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
                        filters: Seq[FilterUnit]): Iterable[RsemGene] = {

      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet].asScala.map { row =>
          RsemGene.readRow(row,
            projection).get
        }
    }

    def getAbundanceData(projection: InputDataModel,
                         filters: Seq[FilterUnit]): Iterable[Abundance] = {

      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet]
        .asScala
        .map { row =>
          Abundance.readRow(row,
            projection).get
        }
    }

    def getIsoformData(projection: InputDataModel,
                       filters: Seq[FilterUnit]): Iterable[RsemIsoform] = {

      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet].asScala.map { row =>
          RsemIsoform.readRow(row,
            projection).get
        }

    }

  }
}
