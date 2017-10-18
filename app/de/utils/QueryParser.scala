package de.utils

import de.model.{ Error, TagError }
import de.model.tags.Formatter
import de.model.tags.Sample.{ Number, Text, Value }
import enumeratum.{ Enum, EnumEntry }
import play.api.libs.json.{ JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue, Json }

// ===========================================================================

sealed trait Query extends Formatter

sealed trait Operation
sealed trait Comparison extends Operation
sealed trait Logical extends Operation

sealed trait Operands
sealed trait Unary extends Operands
sealed trait Nary  extends Operands

sealed trait Result
case class SuccessQuery(value: Query)  extends Result
case class ErrorQuery  (value: Seq[String]) extends Result

// ===========================================================================

sealed trait Operator extends EnumEntry with Operation with Operands
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
    key:      Option[String] = None,
    value:    Seq[Query],
    operator: Operator with Nary,
    not:      Boolean  = false) extends Query {

  override val formatJson = {
    key match {
      case Some(_key) => {
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
          JsArray(Seq(Json.obj(QueryConstants.`tags.key`.entryName -> _key), queryValue)))
      }
      case None => {
        Json.obj(operator.entryName -> JsArray(value.map { _.formatJson }))
      }
    }
   
  }
  
}

// ===========================================================================
case class StringValue(
    value: String) extends Query {
  
  override val formatJson = JsString(value)
  
}

// ===========================================================================
object Queryparser {

  // ---------------------------------------------------------------------------
  private def parseJsBoolean(
              key:   String,
              value: JsBoolean,
              not:   Boolean = false)
          : Result = {
        SuccessQuery(QueryUnit(key, Text(Json.stringify(value)), not = not))
  }
        
  // ---------------------------------------------------------------------------
  private def parseJsString(
              key:      String, 
              value:    JsString, 
              operator: Option[Operator],
              not:      Boolean  = false)
          : Result =  
            operator match {
              case Some(x) => SuccessQuery(QueryUnit(key, Text(value.value), x, not))
              case None    => SuccessQuery(QueryUnit(key, Text(value.value), not=not))
            }
         
  
  // ---------------------------------------------------------------------------
  private def parseJsNumber(
              key:      String,
              value:    JsNumber,
              operator: Option[Operator],
              not:      Boolean  = false)
           : Result = 
             operator match {
              case Some(x) => SuccessQuery(QueryUnit(key, Number(value.value.toString()), x, not))
              case None    => SuccessQuery(QueryUnit(key, Number(value.value.toString()), not=not))
            }
  
  // ---------------------------------------------------------------------------
  private def parseJsObject(
              key:   String, 
              value: JsObject, 
              not:   Boolean  = false)
          : Result = {
    
      Operator.withNameOption(key) match {
        
        case Some(operator) => 
          operator match {
            
            //not operator
            case operation: Logical with Unary  => parseObjectValue(value, not = true)
            
            case operation: Comparison          => parseObjectValue(value, Some(operation), not)
            //for any other operator      
            case _                              => ErrorQuery(Seq(operator.entryName))
         }
        //no nested query objects
        case None         => ErrorQuery(Seq(key))
      }
    }
  // ---------------------------------------------------------------------------
  private def parseJsArray(
              key:   String, 
              value: JsArray, 
              not:   Boolean = false)
          : Result = 

      Operator.withNameOption(key) match {
        case Some(operator) => operator match {
            case operation: Logical with Nary => parseArrayValue(value = value,key=Right(operation), not= not) 
            case _                            => ErrorQuery(Seq(operator.entryName))
        }
        case None           => parseArrayValue(value = value,key=Left(key), not= not) 
      }

  // ---------------------------------------------------------------------------
  private def parseObjectValue(
              value:    JsObject,
              operator: Option[Operator]=None,
              not:      Boolean  = false)
          : Result = {
    
    val result = 
      value
        .value
        .map {
            case (k: String, v: JsString)  => parseJsString(k, v, operator, not=not)
            case (k: String, v: JsNumber)  => parseJsNumber(k, v, operator, not=not)
            case (k: String, v: JsObject)  => parseJsObject(k, v, not=not)
            case (k: String, v: JsArray)   => parseJsArray(k, v, not)
            case (k: String, v: JsBoolean) => parseJsBoolean(k, v, not)
            case (k: String, JsNull)       => throw new IllegalStateException("null not allowed")
        }.toSeq
        
        val errors = result.collect { case a: ErrorQuery => a }
    
    val res = if(errors.isEmpty) {
      val res = result.collect { case a: SuccessQuery => a }
      operator match {
        case Some(x:Logical with Nary)  => SuccessQuery(QueryArray(operator= x, value= res.map { _.value }))
        case _    => SuccessQuery(QueryArray(operator = Operator.$and , value= res.map { _.value }))
      }
     
    } else {
      ErrorQuery(errors.flatMap { _.value })
    }
    res
  }
  
  // ---------------------------------------------------------------------------
  private def parseArrayValue(
              key:   Either[String,Operator with Nary],
              value: JsArray,
              not:   Boolean  = false)
          : Result = {
    
    val result = 
           value
             .value
             .map {
                //not operator cannot have object array as value
                case obj: JsObject if (!not) => parseObjectValue(obj,not =not)
                                        
                case obj: JsString           => SuccessQuery(StringValue(obj.value))
                // for any other JsValue make it as error
                case x                       => ErrorQuery(Seq(Json.stringify(x)))
             }
             
    val errors = result.collect { case a: ErrorQuery => a }
    
    if(errors.isEmpty) {
      val res = result.collect { case a: SuccessQuery => a }
      key match {
        case Left(x) => SuccessQuery(QueryArray(key = Some(x),operator = Operator.$in , value= res.map { _.value }))
        case Right(x) => SuccessQuery(QueryArray(operator= x, value= res.map { _.value }))
      }
     
      
    } else {
      ErrorQuery(errors.flatMap { _.value })
    }
  }

  // ---------------------------------------------------------------------------
  def getMongoQuery(value: JsValue): Either[Error, Query] = {
    
    val result: Result = value match {
       case obj: JsObject => parseObjectValue(obj,operator=None)
       case obj: JsArray  => parseArrayValue(key=Right(Operator.$or),value=obj)
       case _             => ErrorQuery(Seq(Json.stringify(value)))
    }

    result match {
      case a : ErrorQuery   => Left(TagError(a.value))
      case a : SuccessQuery => Right(a.value)
    }
  }
}
// ===========================================================================
