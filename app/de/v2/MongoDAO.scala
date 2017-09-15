package de.v2

import com.fasterxml.jackson.databind.node.ObjectNode
import com.mongodb.MongoClient

import play.api.libs.json.JsObject
import play.api.libs.json.Json
import org.jongo.Jongo

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

 /* private val jongo =
    new org.jongo.Jongo(
      mongoClient
        // warning: see https://github.com/bguerout/jongo/issues/254
        .getDB(databaseName))*/

  override def find(collectionName: String,
                    filters: Map[String, Seq[String]]): Iterable[JsObject] = {

    def stringWithDoubleQuotes(str: String) = s""""$str""""

    def seqAsMongoString(values: Seq[String]) = values
      .map {
        stringWithDoubleQuotes
      }
      .mkString("[", ", ", "]")

    val filter_str =
      filters
        .map {
          case (key, values) =>
            s"""{$key: {$$in: ${seqAsMongoString(values)}}}"""
        }
        .mkString("{ $and : [ ", ", ", " ] }")

    println(filter_str)
    //required, toPlayJsObject throws error for ObjectId type
    val projection_str =
      s"""{_id: 0}"""
    val start = System.currentTimeMillis()

    val to_return = new Iterable[JsObject] {
      override def iterator() = {
        jongo.getCollection(collectionName)
          .find(filter_str)
          .projection(projection_str)
          .as(classOf[ObjectNode])
          .iterator()
          .map(_.toPlayJsObject)
      }
    }
    println("Mongo Execution : "+(System.currentTimeMillis() - start))
    to_return
  }

}
