package test

import scala.io.Source

import org.junit.{ Assert, Test }

import de.utils.Queryparser
import play.api.Application
import play.api.libs.json.Json

class End2EndTest {
  @Test def testEndToEndQueryParser {

    val inputStream  = Application
                        .getClass()
                        .getResourceAsStream("/TestInputJson.txt")
    val inputlines   = Source
                        .fromInputStream(inputStream)
                        .getLines
                        .toSeq

    val outputStream = Application
                        .getClass()
                        .getResourceAsStream("/TestOutputJson.txt")
    val outputlines  = Source
                        .fromInputStream(outputStream)
                        .getLines
                        .toSeq

    for (i <- 0 to inputlines.size-1) {
      val result = Queryparser.getMongoQuery(Json.parse(inputlines(i))).fold(fa => fa, fb => fb.formatJson)
      
      Assert.assertEquals(result, Json.parse(outputlines(i)))
    }
    
  }
}