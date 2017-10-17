package de.utils

import de.model.{ Error, TagError }
import de.model.tags.Formatter
import de.model.tags.Sample.{ Number, Text, Value }
import enumeratum.{ Enum, EnumEntry }
import play.api.libs.json.{ JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue, Json }

// ===========================================================================

sealed trait Query extends Formatter

sealed trait Comparison
sealed trait Logical

sealed trait Operands
sealed trait Unary extends Operands
sealed trait Nary  extends Operands


case class InvalidTag(tag: String)

// ===========================================================================

sealed trait Operator extends EnumEntry
object Operator extends Enum[Operator] {
  val values = findValues

  case object $in  extends Operator with Comparison with Nary
  case object $lt  extends Operator with Comparison with Unary
  case object $lte extends Operator with Comparison with Unary
  case object $gt  extends Operator with Comparison with Unary
  case object $gte extends Operator with Comparison with Unary
  case object $ne  extends Operator with Comparison with Unary
  case object $nin extends Operator with Comparison with Nary

  //as $eq is predefined in scala
  case object eq   extends Operator with Comparison with Unary {
    override def toString = "$eq"
  }

  case object $and extends Operator with Logical with Nary
  case object $or  extends Operator with Logical with Nary
  case object $nor extends Operator with Logical with Nary
  case object $not extends Operator with Logical with Unary

}

sealed trait QueryConstants extends EnumEntry

object QueryConstants extends Enum[QueryConstants] {
  val values = findValues

  case object $and         extends QueryConstants
  case object $not         extends QueryConstants
  case object `tags.key`   extends QueryConstants
  case object `tags.value` extends QueryConstants

}

// ===========================================================================
case class QueryUnit(
    key:      String,
    value:    Value,
    operator: Operator = Operator.eq,
    not:      Boolean  = false) extends Query {

  override val formatJson = {
    val queryValue = 
          if (not) 
            Json.obj(QueryConstants.`tags.value`.entryName -> 
                     Json.obj(QueryConstants.$not.entryName -> 
                              Json.obj(operator.entryName -> value.formatJson)))
          else
            Json.obj(QueryConstants.`tags.value`.entryName -> 
                     Json.obj(operator.entryName -> value.formatJson))

    Json.obj(QueryConstants.$and.entryName -> 
             JsArray(Seq(Json.obj(QueryConstants.`tags.key`.entryName -> key),queryValue)))
  }

}

// ===========================================================================
case class QueryArray(
    key:      String,
    value:    Seq[Query],
    operator: Operator = Operator.$in,
    not:      Boolean  = false) extends Query {

  override val formatJson = {
    val queryTmp = JsArray(value.map { _.formatJson })

    val queryValue = 
      if (not)
        Json.obj(QueryConstants.`tags.value`.entryName -> 
                 Json.obj(QueryConstants.$not.entryName -> 
                          Json.obj(operator.entryName -> queryTmp)))
      else
        Json.obj(QueryConstants.`tags.value`.entryName -> 
                 Json.obj(operator.entryName -> queryTmp))

    Json.obj(QueryConstants.$and.entryName -> 
             JsArray(Seq(Json.obj(QueryConstants.`tags.key`.entryName -> key),queryValue)))
  }
  
}

// ===========================================================================
case class Base(
    key:   String, 
    value: Seq[Query]) extends Query {
  
  override val formatJson =  Json.obj(key -> JsArray(value.map { _.formatJson })) 
  
}

// ===========================================================================
case class StringValue(
    value: String) extends Query {
  
  override val formatJson = JsString(value)
  
}

// ===========================================================================
object Queryparser {

  //TODO: implement ADT for Either foldLeft/foldRight
  private def getResult(
      obj: Seq[Either[Seq[InvalidTag], Seq[Query]]])
                :Either[Seq[InvalidTag], Seq[Query]] = {
    
    val error = obj
      .foldLeft(Seq[InvalidTag]()) {
        (z, acc) => z ++ acc.left.getOrElse(Seq())
      }
    val query = obj
      .foldRight(Seq[Query]()) {
        (z, acc) => acc ++ z.right.getOrElse(Seq())
      }

    if (!error.isEmpty)
      Left(error)
    else
      Right(query)
  }
  
  // ---------------------------------------------------------------------------
  private def parseJsBoolean(
              key:   String,
              value:   JsBoolean,
              not: Boolean = false)
          : Either[Seq[InvalidTag], Seq[Query]] = 
        
        Right(Seq(QueryUnit(key, Text(Json.stringify(value)), not = not)))
        
  // ---------------------------------------------------------------------------
  private def parseJsString(
              key:        String, 
              value:        JsString, 
              operator: Operator = Operator.eq, 
              not:      Boolean  = false)
          : Either[Seq[InvalidTag], Seq[Query]] = 
        
        Right(Seq(QueryUnit(key, Text(value.value), operator, not)))
  
  // ---------------------------------------------------------------------------
  private def parseJsNumber(
              key:        String,
              value:        JsNumber,
              operator: Operator = Operator.eq,
              not:      Boolean  = false)
           : Either[Seq[InvalidTag], Seq[Query]] = 
        
        Right(Seq(QueryUnit(key, Number(value.value.toString()), operator, not)))
  
  // ---------------------------------------------------------------------------
  private def parseJsObject(
              key:        String, 
              value:        JsObject, 
              operator: Operator = Operator.eq, 
              not:      Boolean  = false)
          : Either[Seq[InvalidTag], Seq[Query]] = {
    
      Operator.withNameOption(key) match {
        
        case Some(operator) => 
          operator match {
              case operation: Comparison                 => parseObjectValue(value, operator, not)
              
              case operation: Logical 
                    if(operation.equals(Operator.$not))  => parseObjectValue(value, not = true)
              //for any other operator      
              case _                                     => Left(Seq(InvalidTag(operator.entryName)))
         }
        //no nested query objects
        case None         => Left(Seq(InvalidTag(key))) 
      }
    }
  // ---------------------------------------------------------------------------
  private def parseJsArray(
              key:   String, 
              value:   JsArray, 
              not: Boolean = false)
          : Either[Seq[InvalidTag], Seq[Query]] = 

      Operator.withNameOption(key) match {
        case Some(operator) => operator match {
          
            case operation: Logical with Nary => 
                  parseArrayValue(value, not= not) match {
                    case Left(error)   => Left(error)
                    case Right(result) => Right(Seq(Base(operation.entryName, result)))
                  }
            case _                            => Left(Seq(InvalidTag(operator.entryName))) //for any other operator 
            
          }
        
        case None         =>
                  parseArrayValue(value, not= not) match {
                    case Left(error)   => Left(error)
                    case Right(result) => Right(Seq(QueryArray(key, result)))
                  }
      }

  // ---------------------------------------------------------------------------
  private def parseObjectValue(
              value:    JsObject,
              operator: Operator = Operator.eq,
              not:      Boolean  = false)
          : Either[Seq[InvalidTag], Seq[Query]] = {
    
    val result: Seq[Either[Seq[InvalidTag], Seq[Query]]] = 
      value
        .value
        .map {
            case (k: String, v: JsString)  => parseJsString(k, v, operator, not)
            case (k: String, v: JsNumber)  => parseJsNumber(k, v, operator, not)
            case (k: String, v: JsObject)  => parseJsObject(k, v, not=not)
            case (k: String, v: JsArray)   => parseJsArray(k, v, not)
            case (k: String, v: JsBoolean) => parseJsBoolean(k, v, not)
            case (k: String, JsNull)       => throw new IllegalStateException("null not allowed")
        }
        .toSeq

    getResult(result)
      
  }
  
  // ---------------------------------------------------------------------------
  private def parseArrayValue(
              value:    JsArray,
              operator: Operator = Operator.$in,
              not:      Boolean  = false)
          : Either[Seq[InvalidTag], Seq[Query]] = {
    
    val result = 
           value
             .value
             .map {
                //not operator cannot have object array as value
                case obj: JsObject if (!not) => parseObjectValue(obj,not =not)
                                        
                case obj: JsString           => Right(Seq(StringValue(obj.value)))
                // for any other JsValue make it as error
                case x                       => Left(Seq(InvalidTag(Json.stringify(x))))
             }
    
    getResult(result)
  }

  // ---------------------------------------------------------------------------
  def getMongoQuery(value: JsValue): Either[Error, Query] = {
    
    val result = 
      if (value.isInstanceOf[JsObject])
          parseObjectValue(value.asInstanceOf[JsObject])
      else if (value.isInstanceOf[JsArray])
          parseArrayValue(value.asInstanceOf[JsArray])
      else
          Left(Seq(InvalidTag(Json.stringify(value))))

    result match {
      case Left(errors)  => Left(TagError(errors.map { _.tag }))
      case Right(result) => Right(Base(Operator.$and.entryName, result))
    }
  }
}
// ===========================================================================
