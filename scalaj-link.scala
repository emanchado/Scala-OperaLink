import scalaj.http.{Http, Token}
import scala.collection.mutable.HashMap
import scala.io
import scala.util.parsing.json.{JSON, JSONArray, JSONObject}

package org.demiurgo.operalink {
  class LinkServerProxy(var consumer: Token,
                        var accessToken: Token) {
    val serverBaseUrl = "https://link.api.opera.com/"

    def get(path: String): String = {
      return Http(serverBaseUrl + path).oauth(consumer, accessToken).asString
    }

    def post(path: String, data: String): String = {
      return Http.postData(serverBaseUrl + path, data).
                oauth(consumer, accessToken).asString
    }

    def post(path: String, params: Map[String, String]): String = {
      var request = Http.post(serverBaseUrl + path)
      for ((key,value) <- params) {
        request = request.param(key, value)
      }
      return request.oauth(consumer, accessToken).asString
    }
  }

  class TestLinkServerProxy(consumer: Token, accessToken: Token,
                            fixturePath: String)
        extends LinkServerProxy(consumer, accessToken) {
    var count = 0
    val baseFixturePath = "src/test/fixtures"
    val fixtureSpecJSON =
      io.Source.fromFile(baseFixturePath + "/" + fixturePath + ".json").
          mkString
    val fixtureSpec = JSON.parseRaw(fixtureSpecJSON).get.asInstanceOf[JSONArray]

    def currentStepInfo: Map[Any, Any] = {
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

  abstract class LinkAPIItem(propertySet: JSONObject) {
    val propertyHash = propertySet.obj("properties").asInstanceOf[JSONObject].obj.asInstanceOf[Map[String, String]]
    val baseHash = propertySet.obj.asInstanceOf[Map[String, String]]

    def id: String = baseHash("id")
    def propertyList: Array[String]
  }

  class SpeedDialSlot(propertySet: JSONObject) extends LinkAPIItem(propertySet) {
    def position: String = id
    def propertyList: Array[String] = {
      return Array("uri", "title", "reload_interval", "reload_enabled",
                   "reload_only_if_expired")
    }
    def uri: String = propertyHash("uri")
    def title: String = propertyHash("title")
    def reloadInterval: String = propertyHash("reload_interval")
    def reloadEnabled: String = propertyHash("reload_enabled")
    def reloadOnlyIfExpired: String = propertyHash("reload_only_if_expired")
  }

  class LinkAPI(val consumer: Token, val accessToken: Token) {
    var serverProxy = new LinkServerProxy(consumer, accessToken)

    def getSpeedDial: Seq[SpeedDialSlot] = {
      val json = serverProxy.get("/rest/speeddial/children")
      return for { item <- JSON.parseRaw(json).get.asInstanceOf[JSONArray].list }
             yield new SpeedDialSlot(item.asInstanceOf[JSONObject])
    }

    def getSpeedDialSlot(position: Int): SpeedDialSlot = {
      val json = serverProxy.get("/rest/speeddial/" + position)
      return new SpeedDialSlot(JSON.parseRaw(json).get.asInstanceOf[JSONArray].list(0).asInstanceOf[JSONObject])
    }

    def createSpeedDial(position: Int,
                        properties: Map[String, String]): SpeedDialSlot = {
      val json = serverProxy.post("/rest/speeddial/" + position, properties)
      return new SpeedDialSlot(JSON.parseRaw(json).get.asInstanceOf[JSONArray].list(0).asInstanceOf[JSONObject])
    }
  }
}

object ScalaJLinkTest {
  def main(args: Array[String]) {
    val consumer = Token("8NrRjW2WhWjQIZNbVXZMCWveSkmvQJHn",
                         "BPwJIdhfOvUc91NRe634nyFSPPHYTDrx")
    var accessToken : Token = null

    if (args.length == 2) {
      accessToken = Token(args(0), args(1))
    } else {
      try {
        val token = Http("https://auth.opera.com/service/oauth/request_token").param("oauth_callback","oob").oauth(consumer).asToken

        println("Go to https://auth.opera.com/service/oauth/authorize?oauth_token=" + token.key)
        val verifier = Console.readLine("Enter verifier: ").trim

        accessToken = Http("https://auth.opera.com/service/oauth/access_token").oauth(consumer, token, verifier).asToken
        println("I got an access token! Namely, " + accessToken.key + " / " + accessToken.secret)
      } catch {
        case e: scalaj.http.HttpException => println(e.body); System.exit(1)
      }
    }

    /* println(Http("https://link.api.opera.com/rest/speeddial/children/").header("content-type", "application/json").oauth(consumer, accessToken).asString) */
    println(Http("https://link.api.opera.com/rest/speeddial/children/").oauth(consumer, accessToken).asString)
  }
}
