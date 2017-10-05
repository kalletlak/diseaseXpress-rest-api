package de.dao

import org.jongo.Jongo
import com.fasterxml.jackson.databind.node.ObjectNode
import de.model.Inputs.FilterUnit
import play.api.libs.json.{ JsObject, Json }
import de.repository.Dao

class MongoDAO(
    jongo: Jongo) extends Dao {

  import scala.collection.JavaConversions.asScalaIterator

  implicit private class ObjectNodeImplicits(objectNode: ObjectNode) {
    def toPlayJsObject: JsObject = {
      Option(objectNode) match {
        case None =>
          throw new IllegalStateException // TODO: proper handling (likely missing collection)

        case Some(obj) =>
          Json
            .parse(obj.toString)
            .as[JsObject]
      }
    }

  }

  override def find(collectionName: String,
                    filters: Seq[FilterUnit]): Iterable[JsObject] = {

    //required, toPlayJsObject throws error for ObjectId type

    val filters_str = filters
      .map { _.queryMongoString }
      .mkString("{ $and : [ ", ", ", " ] }")

    // set _id: 0 since converting it to JsObject throws an error
    val projection_str =
      s"""{_id: 0}"""

    new Iterable[JsObject] {
      override def iterator() = {
        jongo.getCollection(collectionName)
          .find(filters_str)
          .projection(projection_str)
          .as(classOf[ObjectNode])
          .iterator()
          .map(_.toPlayJsObject)
      }
    }
  }

}
