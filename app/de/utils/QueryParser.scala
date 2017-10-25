package de.utils

import de.model.{ Error, TagError }
import de.model.tags.Formatter
import de.model.tags.Sample.{ Number, Text, Value }
import enumeratum.{ Enum, EnumEntry }
import play.api.libs.json.{ JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue, Json }

// ===========================================================================
object Queryparser {
  
  sealed trait Operator
  sealed trait Comparison extends Operator
  sealed trait Logical    extends Operator
  
  sealed trait Operands
  sealed trait Unary extends Operands
  sealed trait Nary  extends Operands
  
// ===========================================================================

  sealed trait Operation extends EnumEntry with Operator with Operands
  object Operation extends Enum[Operation] {
    val values = findValues
  
    case object $in  extends Operation with Comparison with Nary
    case object $lt  extends Operation with Comparison with Unary
    case object $lte extends Operation with Comparison with Unary
    case object $gt  extends Operation with Comparison with Unary
    case object $gte extends Operation with Comparison with Unary
    case object $ne  extends Operation with Comparison with Unary
    case object $nin extends Operation with Comparison with Nary
  
    //as $eq is predefined in scala
    case object eq   extends Operation with Comparison with Unary {
      override def toString = "$eq"
    }
  
    case object $and extends Operation with Logical with Nary
    case object $or  extends Operation with Logical with Nary
    case object $nor extends Operation with Logical with Nary
    case object $not extends Operation with Logical with Unary
  
  }

  sealed trait QueryConstants extends EnumEntry

  object QueryConstants extends Enum[QueryConstants] {
    val values = findValues
  
    case object $and         extends QueryConstants
    case object $not         extends QueryConstants
    case object `tags.key`   extends QueryConstants
    case object `tags.value` extends QueryConstants
  
  }
  
  sealed trait Formatter {
    def formatJson: JsValue
  }
  sealed trait QueryKey   extends Formatter
  sealed trait QueryValue extends Formatter
  
  sealed trait Query extends Formatter{
    val key       : QueryKey   = _Key("")
    val value     : QueryValue = _Value(Text(""))
    val operation : Operation  = Operation.eq
  }
  
  sealed trait Result
  case class SuccessQuery(value: Query)       extends Result
  case class ErrorQuery  (value: Seq[String]) extends Result
    
    
  case class _Key(obj:String)  extends QueryKey{
    def formatJson = Json.obj(QueryConstants.`tags.key`.entryName -> obj)
  }
  
  case class _Value(obj:Value) extends QueryValue{
    def formatJson = obj.formatJson
  }
  
  case class QueryAsValue(obj: Query)         extends QueryValue{
    def formatJson = Json.obj(obj.operation.entryName ->  obj.value.formatJson)
  }
  
  case class QueriesAsValues(obj: Seq[Query]) extends QueryValue{
    def formatJson =JsArray(obj.map { _.formatJson })
  }
  
  case class ValueAsQuery(override val value:QueryValue) extends Query {
    def formatJson =  value.formatJson
  }
    
  case class QueryKV(
                  override val key:      _Key, 
                  override val value:     QueryValue,
                  override val operation: Operation=Operation.eq) extends Query {
    
    override def formatJson =  {
      Json.obj(QueryConstants.$and.entryName -> 
               JsArray( Seq(key.formatJson, 
                            Json.obj(QueryConstants.`tags.value`.entryName ->  
                                     Json.obj(operation.entryName ->  value.formatJson)))))
                 
    }
  }
  
  case class QueryObject(
                obj:                    Query,
                override val operation: Operation) extends Query {
    
    override val key = obj.key
    override val value = operation match {
      case (x:Comparison) => obj.value
      case (x:Logical)    => QueryAsValue(obj)
    }
    
    override def formatJson =  {
      Json.obj(QueryConstants.$and.entryName -> 
               JsArray( Seq(key.formatJson, 
                            Json.obj(QueryConstants.`tags.value`.entryName ->  
                                     Json.obj(operation.entryName ->  value.formatJson)))))
               
    }
  }

  case class QueryArray(
          obj:                    Seq[Query],
          override val operation: Operation with Logical with Nary = Operation.$and) extends Query {
    
    override def formatJson =
      //this is to remove noise, unwanted $and tags
      if (obj.size == 1) {
        obj.head.formatJson
      } else {
        Json.obj(operation.entryName -> JsArray(obj.map { x => x.formatJson }))
      }
    
  }
  // ---------------------------------------------------------------------------
  private def parseJsString(
              key:      String, 
              value:    JsString)
          : Result = SuccessQuery(QueryKV(key = _Key(key), _Value(Text(value.value))))
          
  // ---------------------------------------------------------------------------
  private def parseJsNumber(
              key:      String, 
              value:    JsNumber)
          : Result = SuccessQuery(QueryKV(key = _Key(key), _Value(Number(value.value.toString()))))
          
          
  // ---------------------------------------------------------------------------
  private def parseJsArray(
              key:      String, 
              value:    JsArray,
              parentOperator: Option[Operation])
          : Result = {

      def apply = Operation.withNameOption(key) match {
        
        case Some(operator) => 
          operator match {
            
            case operation: Logical with Nary => parseArrayValue(value = value,operator = operation)
            
            case _                            => ErrorQuery(Seq(operator.entryName))
        }
        case None           => parseArrayValue(value = value,operator=key)
      }
      
      parentOperator match {
                case Some(operation : Operation with Nary)  => apply
                case Some(operation : Operation with Unary) => ErrorQuery(Seq(operation.entryName))
                case None                                   => apply
      }
  }
         
  // ---------------------------------------------------------------------------
  private def parseJsObject(
              key:      String, 
              value:    JsObject,
              parentOperator: Option[Operation])
          : Result = {
    
     def apply = Operation.withNameOption(key) match {
        
        case Some(operator) => 
          operator match {
            
            //not operator
            case operation: Logical      => parseObjectValue(value, Some(operation))
            
            case operation: Comparison   => parseObjectValue(value, Some(operation))
            //for any other operator      
            case _                       => ErrorQuery(Seq(operator.entryName))
         }
        //no nested query objects
        case None           => ErrorQuery(Seq(key))
      }
     
     parentOperator match {
                case Some(x : Operation with Logical with Unary)    => apply
                case Some(x : Operation with Comparison with Unary) => ErrorQuery(Seq(x.entryName))
                case Some(x : Operation with Nary)                  => apply
                case None                                           => apply
     }
  }

  
  // ---------------------------------------------------------------------------
  //ex { "$or": [{ "$eq": { "mycn_status":"amplified" }},{ "stage": "4" } ] }
  private def parseArrayValue(
              value:    JsArray,
              operator: Operation with Logical with Nary = Operation.$or)
          : Result = {
    
    val result = 
           value
             .value
             .map {
                //not operator cannot have object array as value
                case obj: JsObject => parseObjectValue(obj)
                
                // for any other JsValue make it as error
                case anyJsValue    => ErrorQuery(Seq(Json.stringify(anyJsValue)))
             }

     prepResult(result,Some(operator))
  }
 
  // ---------------------------------------------------------------------------
  //ex { "risk": ["high","low"] }
  private def parseArrayValue(
              operator: String,
              value:    JsArray)
          : Result = {
    
    val result = 
           value
             .value
             .map {
                case obj: JsString  => SuccessQuery(ValueAsQuery(_Value(Text(obj.value))))
                
                case obj: JsNumber  => SuccessQuery(ValueAsQuery(_Value(Number(obj.value.toString()))))
                // for any other JsValue make it as error
                case anyJsValue     => ErrorQuery(Seq(Json.stringify(anyJsValue)))
             }
             
    val errors = result.collect { case a: ErrorQuery => a }
    
    if(errors.isEmpty) {
      val res = result.collect { case a: SuccessQuery => a }
      SuccessQuery(QueryKV(
                      key       = _Key(operator),
                      value     = QueriesAsValues(res.map ( _.value )), 
                      operation = Operation.$in))
    } else {
      ErrorQuery(errors.flatMap ( _.value ))
    }
  }
  
  // ---------------------------------------------------------------------------
  private def prepResult(
              result:   Seq[Result], 
              operator: Option[Operation]       = None): Result = {
    
    val errors = result.collect { case a: ErrorQuery => a }.flatMap ( _.value )
    
    if(errors.isEmpty) {
      val res = result.collect { case a: SuccessQuery => a }.map ( _.value )
      operator match {
        //for Unary operations result would be of length one 
        case Some(operation: Operation with Comparison)         => SuccessQuery(
                                                                      QueryObject(
                                                                          obj       = res.head,
                                                                          operation = operation))
        //for Unary operations result would be of length one 
        case Some(operation: Operation with Logical with Unary) => SuccessQuery(
                                                                      QueryObject(
                                                                          obj       = res.head, 
                                                                          operation = operation))
        case Some(operation: Operation with Logical with Nary)  => SuccessQuery(
                                                                      QueryArray(
                                                                        obj       = res,
                                                                        operation = operation))
        //ex: { "stage": "4",  "mycn_status":"amplified"  }
        case None                                               => SuccessQuery(
                                                                      QueryArray(res))
      }
     
    } else {
      ErrorQuery(errors)
    }
  }

  // ---------------------------------------------------------------------------
  private def parseObjectValue(
              value:    JsObject,
              operator: Option[Operation]=None)
          : Result = {
    
    val result = 
      value
        .value
        .map {
            case (k: String, v: JsString)  => parseJsString(k, v)
            case (k: String, v: JsNumber)  => parseJsNumber(k, v)
            case (k: String, v: JsObject)  => parseJsObject(k, v, operator)
            case (k: String, v: JsArray)   => parseJsArray (k, v, operator)
            
            case (k: String, _)       => throw new IllegalStateException("null not allowed")
        }.toSeq
        
    prepResult(result,operator)
  }

  // ---------------------------------------------------------------------------
  def getMongoQuery(value: JsValue): Either[Error, Query] = {
    
    val result: Result = value match {
       case obj: JsObject => parseObjectValue(obj)
       case obj: JsArray  => parseArrayValue(obj)
       case _             => ErrorQuery(Seq(Json.stringify(value)))
    }

    result match {
      case a : ErrorQuery   => Left(TagError(a.value))
      case a : SuccessQuery => Right(a.value)
    }
  }
}
// ===========================================================================
