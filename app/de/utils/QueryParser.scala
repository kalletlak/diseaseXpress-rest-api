package de.utils

import de.model.{ Error, TagError }
import de.model.tags.Formatter
import de.model.tags.Sample.{ Number, Text, Value }
import enumeratum.{ Enum, EnumEntry }
import play.api.libs.json.{ JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue }
// ===========================================================================

trait Query extends Formatter

trait Comparison
trait SingleValue
trait ArrayValue
trait Logical

sealed case class InvalidTag(tag: String)

// ===========================================================================

sealed trait Operator extends EnumEntry
object Operator extends Enum[Operator] {
  val values = findValues

  case object $in  extends Operator with Comparison with ArrayValue
  case object $lt  extends Operator with Comparison with SingleValue
  case object $lte extends Operator with Comparison with SingleValue
  case object $gt  extends Operator with Comparison with SingleValue
  case object $gte extends Operator with Comparison with SingleValue
  case object $ne  extends Operator with Comparison with SingleValue
  case object $nin extends Operator with Comparison with ArrayValue

  //as $eq is predefined in scala
  case object eq   extends Operator with Comparison with SingleValue {
    override def toString = "$eq"
  }

  case object $and extends Operator with Logical with ArrayValue
  case object $or  extends Operator with Logical with ArrayValue
  case object $nor extends Operator with Logical with ArrayValue
  case object $not extends Operator with Logical with SingleValue

}

sealed trait QueryConstants extends EnumEntry

object QueryConstants extends Enum[QueryConstants] {
  val values = findValues

  case object $and         extends QueryConstants{
    override def toString = s"""$$and"""
  }
  case object $not         extends QueryConstants{
    override def toString = s"""$$not"""
  }
  case object tagskey   extends QueryConstants{
    override def toString = s""""tags.key""""
  }
  case object tagsvalue extends QueryConstants{
    override def toString = s""""tags.value""""
  }

}

// ===========================================================================
case class QueryUnit(
    key:      String,
    value:    Value,
    operator: Operator = Operator.eq,
    not:      Boolean  = false) extends Query {

  override val formatQuery = {
    val queryTmp = 
          if (not) 
              s"""{ ${QueryConstants.tagsvalue}: { ${QueryConstants.$not}: { ${operator.entryName}: ${value.formatQuery} } } }"""
          else 
              s"""{ ${QueryConstants.tagsvalue}: { ${operator.entryName}: ${value.formatQuery} } }"""

    s"""{ ${QueryConstants.$and}: [ {  ${QueryConstants.tagskey}: "$key" }, $queryTmp ]}"""
  }
  override val formatJson = JsString(formatQuery)

}

// ===========================================================================
case class QueryArray(
    key:      String,
    value:    Seq[Query],
    operator: Operator = Operator.$in,
    not:      Boolean  = false) extends Query {

  override val formatQuery = {
    val queryValue = value.map { _.formatQuery }.mkString("[", ",", "]")

    val queryTmp = 
      if (not) 
          s"""{ ${QueryConstants.tagsvalue}: { ${QueryConstants.$not}: { ${operator.entryName}: $queryValue } } }"""
      else 
          s"""{ ${QueryConstants.tagsvalue}: { ${operator.entryName}: $queryValue } }"""
    
    s"""{${QueryConstants.$and}: [{${QueryConstants.tagskey}: "$key"}, $queryTmp]}"""
  }
  override val formatJson = JsString(formatQuery)
  
}

// ===========================================================================
case class QueryObject(
    key:   String, 
    value: Query) extends Query {
  
  val queryValue = value.formatQuery
  override val formatQuery = s"""$queryValue"""
  override val formatJson = JsString(formatQuery)
  
}

// ===========================================================================
case class Base(
    key:   String, 
    value: Seq[Query]) extends Query {
  
  val queryValue = value.map { _.formatQuery }.mkString("[", ",", "]")
  override val formatQuery = s"""{$key : $queryValue}"""
  override val formatJson = JsString(formatQuery)
  
}

// ===========================================================================
case class StringValue(
    value: String) extends Query {
  
  override val formatQuery = s""""$value""""
  override val formatJson = JsString(formatQuery)
  
}

// ===========================================================================
object Queryparser {

  private def getResult(obj: Seq[Either[Seq[InvalidTag], Seq[Query]]]) = {
    
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
              k:   String,
              v:   JsBoolean,
              not: Boolean = false): Either[Seq[InvalidTag], Seq[Query]] = 
        
        Right(Seq(QueryUnit(k, Text(v.value.toString), not = not)))
        
  // ---------------------------------------------------------------------------
  private def parseJsString(
              k:        String, 
              v:        JsString, 
              operator: Operator = Operator.eq, 
              not:      Boolean  = false): Either[Seq[InvalidTag], Seq[Query]] = 
        
        Right(Seq(QueryUnit(k, Text(v.value), operator, not)))
  
  // ---------------------------------------------------------------------------
  private def parseJsNumber(
      k:        String,
      v:        JsNumber,
      operator: Operator = Operator.eq,
      not:      Boolean  = false): Either[Seq[InvalidTag], Seq[Query]] = 
        
        Right(Seq(QueryUnit(k, Number(v.value.toString), operator, not)))
  
  // ---------------------------------------------------------------------------
  private def parseJsObject(
        k:        String, 
        v:        JsObject, 
        operator: Operator = Operator.eq, 
        not:      Boolean  = false): Either[Seq[InvalidTag], Seq[Query]] = {
    
      Operator.withNameOption(k) match {
        
        case Some(operator) => 
          operator match {
              case operation: Comparison => parseObjectValue(v, operator, not)
              case operation: Logical    => 
                  if (not) 
                    Left(Seq(InvalidTag(operation.entryName)))
                  else 
                    operation match {
                       case Operator.$not => parseObjectValue(v, not = true)
                       case _             => Left(Seq(InvalidTag(operator.entryName)))//for any other operator
                    }
         }
        //no nested query objects
        case None         => Left(Seq(InvalidTag(k))) 
      }
    }
  // ---------------------------------------------------------------------------
  private def parseJsArray(
        k:   String, 
        v:   JsArray, 
        not: Boolean = false): Either[Seq[InvalidTag], Seq[Query]] = 

      Operator.withNameOption(k) match {
        case Some(operator) => operator match {
          
            case operation: Logical with ArrayValue => 
                  parseArrayValue(v, not= not) match {
                    case Left(error)   => Left(error)
                    case Right(result) => Right(Seq(Base(operation.entryName, result)))
                  }
            case _                                  => Left(Seq(InvalidTag(operator.entryName))) //for any other operator 
            
          }
        
        case None         =>
                  parseArrayValue(v, not= not) match {
                    case Left(error)   => Left(error)
                    case Right(result) => Right(Seq(QueryArray(k, result)))
                  }
      }

  // ---------------------------------------------------------------------------
  private def parseObjectValue(
      value:    JsObject,
      operator: Operator = Operator.eq,
      not:      Boolean  = false): Either[Seq[InvalidTag], Seq[Query]] = {
    
    val result: Seq[Either[Seq[InvalidTag], Seq[Query]]] = 
      value
        .value
        .map {
            case (k: String, v: JsString)  => parseJsString(k, v, operator, not)
            case (k: String, v: JsNumber)  => parseJsNumber(k, v, operator, not)
            case (k: String, v: JsObject)  => parseJsObject(k, v, not=not)
            case (k: String, v: JsArray)   => parseJsArray(k, v, not)
            case (k: String, v: JsBoolean) => parseJsBoolean(k, v, not)
            case (k: String, JsNull)       => Left(Seq(InvalidTag(JsNull.toString()))) // TODO: is null allowed?
        }
        .toSeq

    getResult(result)
      
  }
  
  // ---------------------------------------------------------------------------
  private def parseArrayValue(
      value:    JsArray,
      operator: Operator = Operator.$in,
      not:      Boolean  = false): Either[Seq[InvalidTag], Seq[Query]] = {
    
    val result = 
           value
             .value
             .map {
                case obj: JsObject => if (not) 
                                        Left(Seq(InvalidTag(obj.toString))) 
                                      else 
                                        parseObjectValue(obj,not =not)
                                        
                case obj: JsString => Right(Seq(StringValue(obj.value)))
                
                case x             => Left(Seq(InvalidTag(x.toString))) // for any other JsValue make it as error
             }
    
    getResult(result)
  }

  // ---------------------------------------------------------------------------
  def getMongoQuery(
      value: JsValue): Either[Error, Query] = {
    
    val result = 
      if (value.isInstanceOf[JsObject])
          parseObjectValue(value.asInstanceOf[JsObject])
      else if (value.isInstanceOf[JsArray])
          parseArrayValue(value.asInstanceOf[JsArray])
      else
          Left(Seq(InvalidTag(value.toString)))

    result match {
      case Left(errors)  => Left(TagError(errors.map { _.tag }))
      case Right(result) => Right(Base(Operator.$and.entryName, result))
    }
  }
}
// ===========================================================================
