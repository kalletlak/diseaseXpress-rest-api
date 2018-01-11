package utils

//TODO: temporary methods
object OptionUtils {
  def arrayToString(obj: Option[Seq[String]]) = 
      if(obj.isDefined)
        obj
          .get
          .mkString(",") 
      else 
        s""""""
}