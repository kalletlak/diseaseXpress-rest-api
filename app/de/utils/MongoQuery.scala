package de.utils

import de.model.Domain.{ Formatter, Number, Text, Value }
import enumeratum.{ Enum, EnumEntry }
import play.api.libs.json.{ JsArray, JsNumber, JsObject, JsString, JsValue, Json }

trait Query extends Formatter
trait Comparison
trait Logical

case class InvalidQueryException(private val message: String = "",
                                 private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

sealed trait OperationType extends EnumEntry
object OperationType extends Enum[OperationType] {
  val values = findValues

  case object $in extends OperationType with Comparison
  case object $lt extends OperationType with Comparison
  case object $lte extends OperationType with Comparison
  case object $gt extends OperationType with Comparison
  case object $gte extends OperationType with Comparison
  case object $ne extends OperationType with Comparison
  case object $nin extends OperationType with Comparison

  //as $eq is predefined in scala
  case object eq extends OperationType with Comparison {
    override def toString = "$eq"
  }

  case object $and extends OperationType with Logical
  case object $or extends OperationType with Logical
  case object $nor extends OperationType with Logical
  case object $not extends OperationType with Logical

}

case class QueryUnit(key: String, value: Value, operationType: Option[OperationType]) extends Query {

  val queryValue = value.formatQuery
  override val formatQuery = operationType match {
    case Some(x) => {
      val operator = x.entryName
      s"""{$$and: [{"tags.key": "$key"}, {"tags.value": {$operator: $queryValue}}]}"""
    }
    //this is for simple equal query, example """{"Risk": "Low"}"""
    case None => s"""{$$and: [{"tags.key": "$key"}, {"tags.value": $queryValue}]}"""
  }
  override val formatJson = JsString(formatQuery)

}

case class QueryArray(key: String, value: Seq[Query], operationType: Option[OperationType]) extends Query {

  override val formatQuery = operationType match {
    case Some(x) => {
      val operator = x.entryName
      val queryValue = value.map { x =>
        {
          val queryValue = x.formatQuery
          s""""$queryValue""""
        }
      }.mkString("[", ",", "]")
      s"""{$$and: [{"tags.key": "$key"}, {"tags.value": {$operator: "$queryValue"}}]}"""
    }
    //this is for simple equal query, example """{"Risk": "Low"}"""
    case None => {
      val queryValue = value.map { x =>
        {
          val queryValue = x.formatQuery
          s""""$queryValue""""
        }
      }.mkString("[", ",", "]")
      s"""{$$and: [{"tags.key": "$key"}, {"tags.value": {$$in: $queryValue}}]}"""
    }
  }
  override val formatJson = JsString(formatQuery)
}

case class QueryObject(key: String, value: Query) extends Query {
  val queryValue = value.formatQuery
  override val formatQuery = s"""$queryValue"""
  override val formatJson = JsString(formatQuery)
}

case class Base(key: String, value: Seq[Query]) extends Query {
  val queryKey = key
  val queryValue = value.map { _.formatQuery }.mkString("[", ",", "]")
  override val formatQuery = s"""{$queryKey : $queryValue}"""
  override val formatJson = JsString(formatQuery)
}

case class StringValue(value: String) extends Query {
  override val formatQuery = value
  override val formatJson = JsString(formatQuery)
}

object MongoQuery {
  private def parseJsArray(value: JsArray, withTag: Option[OperationType]): Seq[Query] = {
    value.value.flatMap {
      x =>
        x match {
          case x: JsObject => Some(parseJsObject(x, withTag))
          case x: JsString => Some(Seq(StringValue(x.value)))
          case _           => None
        }
    }
      .flatten
  }

  private def parseNotQuery(v: JsObject) = {

    val temp = v
      .value
      .flatMap {
        case (k1: String, v1: JsObject) => {
          OperationType.withNameOption(k1) match {
            case Some(operation1: Comparison) => {
              val temp = v1.value.map {
                case (kx: String, vx: JsString) => {
                  val queryValue = Text(vx.value).formatQuery
                  Some(StringValue(s"""{$$and: [{"tags.key": "$kx"}, {"tags.value": {$$not:{$operation1: $queryValue}}}]}"""))
                }
                case (kx: String, vx: JsNumber) => {
                  val queryValue = Number(vx.value.toString).formatQuery
                  Some(StringValue(s"""{$$and: [{"tags.key": "$kx"}, {"tags.value": {$$not:{$operation1: $queryValue}}}]}"""))
                }
                case (kx: String, vx: JsArray) => {

                  val temp = vx.value.flatMap {
                    _ match {
                      case x: JsString => Some(Seq(StringValue(x.value)))
                      case x: Any => {
                        assert(false, InvalidQueryException(s"""$x should be a string"""))
                        None
                      }
                    }
                  }
                    .flatten
                  val t1 = temp
                    .map { x => s""""${x.formatQuery}"""" }
                    .mkString("[", ",", "]")
                  Some(StringValue(s"""{$$and: [{"tags.key": "$kx"}, {"tags.value": {$$not:{$operation1: $t1}}}]}"""))
                }
                case x: Any => {
                  assert(false, InvalidQueryException(s"""invalid identifier : $x"""))
                  None
                }

              }.flatten.toSeq
              Some(temp)
            }
            case x: Any => {
              assert(false, throw new InvalidQueryException(s"""$x should be comparison operator"""))
              None
            }
          }
        }
        case x: Any => {
          assert(false, InvalidQueryException(s"""invalid $x"""))
          None
        }
      }.flatten.toSeq.head
    Some(temp)

  }

  private def parseJsObject(value: JsObject, withTag: Option[OperationType]): Seq[Query] = {
    value
      .value
      .map {
        case (k: String, v: JsString) => {
          Some(QueryUnit(k, Text(v.value), withTag))
        }
        case (k: String, v: JsNumber) => {
          Some(QueryUnit(k, Number(v.value.toString), withTag))
        }
        case (k: String, v: JsObject) => {
          OperationType.withNameOption(k) match {
            case Some(x) => {
              x match {
                case operation: Comparison => {
                  Some(QueryObject(operation.entryName, parseJsObject(v, Some(operation)).head))
                }
                //TODO: other operators
                case OperationType.$not => { parseNotQuery(v) }
                case operation: Logical => {
                  val x = operation.entryName
                  assert(false, InvalidQueryException(s"""invalid cannot be logical : $x"""))
                  ???
                }
              }
            }
            //no nested query objects
            case x: Any => {
              assert(false, InvalidQueryException(s"""invalid operator : $x"""))
              ???
            }
          }
        }
        case (k: String, v: JsArray) => {
          OperationType.withNameOption(k) match {
            case Some(x) => {
              x match {
                case operation: Comparison => {
                  Some(QueryArray(operation.entryName, parseJsArray(v, Some(operation)), Some(operation)))
                }
                case operation: Logical => {
                  Some(Base(operation.entryName, parseJsArray(v, None)))
                }
              }
            }
            case _ => {
              Some(QueryArray(k, parseJsArray(v, None), None))
            }
          }
        }
        case x: Any => {
          assert(false, InvalidQueryException(s"""invalid : $x"""))
          ???
        }
      }
      .toSeq
      .flatten
  }
  def getMongoQuery(value: JsValue): Query = {
    value match {
      case obj: JsObject => parseJsObject(obj, None).head
      case _ => {
        assert(false, InvalidQueryException(s"""$value should be a object"""))
        ???
      }
    }
  }
}
