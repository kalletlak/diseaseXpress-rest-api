package de.service

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

import com.datastax.driver.core.ResultSet

import de.model.input.{AbundanceProjectons, RsemIsoformProjectons, RsemGeneProjectons, FilterUnit, InputDataModel}
import de.model.output.{Abundance, RsemGene, RsemIsoform}
import de.repository.Repository
import io.swagger.annotations.ApiModel

// ===========================================================================
trait CassandraService extends ServiceComponent {
  this: Repository =>

  def service = new DefaultService

  // ---------------------------------------------------------------------------
  final class DefaultService extends Service {
    
    override def find(
        collectionName: String,
        filters:        Seq[FilterUnit]) =
      dao.find(collectionName, filters)

    // ---------------------------------------------------------------------------
    def getRsemGeneData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[RsemGene] =
      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet]
        .asScala
        .map(RsemGene.fromCassandra(
          projection.asInstanceOf[RsemGeneProjectons]))
    
    // ---------------------------------------------------------------------------
    def getAbundanceData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[Abundance] =
      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet]
        .asScala
        .map(Abundance.fromCassandra(
          projection.asInstanceOf[AbundanceProjectons]))    
    
    // ---------------------------------------------------------------------------
    def getIsoformData(
          projection: InputDataModel,
          filters:    Seq[FilterUnit])
        : Iterable[RsemIsoform] =
      find(projection.collection_name, filters)
        .asInstanceOf[ResultSet]
        .asScala
        .map(RsemIsoform.fromCassandra(
          projection.asInstanceOf[RsemIsoformProjectons]))

  }
}

// ===========================================================================
