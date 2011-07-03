import scalaj.http.{Http, Token}
import scala.util.parsing.json.{JSON, JSONArray, JSONObject}

package org.demiurgo.operalink {
  class TestLinkServerProxy(consumer: Token, accessToken: Token,
                            fixturePath: String)
        extends LinkServerProxy(consumer, accessToken) {
    var count = 0
    val baseFixturePath = "src/test/resources"
    val fixtureSpecJSON =
      io.Source.fromFile(baseFixturePath + "/" + fixturePath + ".json").
          mkString
    val fixtureSpec = JSON.parseRaw(fixtureSpecJSON).get.asInstanceOf[JSONArray]

    def currentStepInfo: Map[String, Any] = {
      return fixtureSpec.list(count - 1).asInstanceOf[JSONObject].obj
    }

    def cannedResponse: String = {
      return currentStepInfo.asInstanceOf[Map[String, String]]("response")
    }

    def checkRequest(reqType: String, path: String): Boolean = {
      val expectedReqType = currentStepInfo.asInstanceOf[Map[String, String]]("method")
      val expectedPath = currentStepInfo.asInstanceOf[Map[String, String]]("path")
      if (expectedReqType != reqType) {
        throw new Exception("Expected request type '" + expectedReqType +
                              "', got '" + reqType + "'")
      }
      if (expectedPath != path) {
        throw new Exception("Expected URL path '" + expectedPath +
                              "', got '" + path + "'")
      }
      return true;
    }

    def checkRequest(reqType: String, path: String, data: String): Boolean = {
      val expectedReqType = currentStepInfo.asInstanceOf[Map[String, String]]("method")
      val expectedPath = currentStepInfo.asInstanceOf[Map[String, String]]("path")
      val expectedData = currentStepInfo.asInstanceOf[Map[String, JSONObject]]("data")
      if (expectedReqType != reqType) {
        throw new Exception("Expected request type '" + expectedReqType +
                              "', got '" + reqType + "'")
      }
      if (expectedPath != path) {
        throw new Exception("Expected URL path '" + expectedPath +
                              "', got '" + path + "'")
      }
      if (expectedData != data) {
        throw new Exception("Expected POST data '" + expectedData +
                              "', got '" + data + "'")
      }
      return true;
    }

    def checkRequest(reqType: String, path: String, params: Map[String, String]): Boolean = {
      val expectedReqType = currentStepInfo.asInstanceOf[Map[String, String]]("method")
      val expectedPath = currentStepInfo.asInstanceOf[Map[String, String]]("path")
      val expectedParams = currentStepInfo.asInstanceOf[Map[String, JSONObject]]("params").obj.asInstanceOf[Map[String, String]]
      if (expectedReqType != reqType) {
        throw new Exception("Expected request type '" + expectedReqType +
                              "', got '" + reqType + "'")
      }
      if (expectedPath != path) {
        throw new Exception("Expected URL path '" + expectedPath +
                              "', got '" + path + "'")
      }
      if (! expectedParams.equals(params)) {
        throw new Exception("Expected POST parameters '" + expectedParams +
                              "', got '" + params + "'")
      }
      return true;
    }

    override def get(path: String): String = {
      count += 1
      checkRequest("get", path)
      return cannedResponse
    }

    override def post(path: String, data: String): String = {
      count += 1
      checkRequest("post", path, data)
      return cannedResponse
    }

    override def post(path: String, params: Map[String, String]): String = {
      count += 1
      checkRequest("post", path, params)
      return cannedResponse
    }
  }
}
