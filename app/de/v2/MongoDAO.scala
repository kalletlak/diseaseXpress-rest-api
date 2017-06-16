package de.v2

import com.fasterxml.jackson.databind.node.ObjectNode
import com.mongodb.MongoClient

import play.api.libs.json.JsObject
import play.api.libs.json.Json

class MongoDAO(
    private val mongoClient: MongoClient,
    databaseName: String) {

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
  val database = mongoClient.getDatabase(databaseName)

  private val jongo =
    new org.jongo.Jongo(
      mongoClient
        // warning: see https://github.com/bguerout/jongo/issues/254
        .getDB(databaseName))

  def findData(collectionName: String, filters: Map[String, Seq[String]]): Iterable[JsObject] = {
    val filter_str = filters
      .map { case (name, values) => s"""{$name: {$$in: ${values.map { x => s""""$x"""" }.mkString("[", ", ", "]")}}}""" }
      .mkString("{ $and : [ ", ", ", " ] }")

    //required, toPlayJsObject throws error for ObjectId type
    val projection_str = s"""{_id: 0}"""
    new Iterable[JsObject] {
      override def iterator() = {
        jongo.getCollection(collectionName)
          .find(filter_str)
          .projection(projection_str)
          .as(classOf[ObjectNode])
          .iterator()
          .map(_.toPlayJsObject)
      }
    }
  }

}
