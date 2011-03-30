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
                            fixturePath: String = "src/test/fixtures")
        extends LinkServerProxy(consumer, accessToken) {
    val count = new HashMap[String, Int]

    def cannedResponse(reqType: String): String = {
      val c = count.get(reqType).getOrElse(0) + 1
      count(reqType) = c
      return io.Source.fromFile(fixturePath + "/" +
                                  reqType + "-" + c + ".json").
                       mkString
    }

    override def get(path: String): String = {
      return cannedResponse("get")
    }

    override def post(path: String, data: String): String = {
      return cannedResponse("postdata")
    }

    override def post(path: String, params: Map[String, String]): String = {
      return cannedResponse("postparams")
    }
  }

  abstract class LinkAPIItem(propertySet: JSONObject) {
    val propertyHash = propertySet.obj("properties").asInstanceOf[JSONObject].obj.asInstanceOf[Map[String, String]]
    val baseHash = propertySet.obj.asInstanceOf[Map[String, String]]

    def id: String = baseHash("id")
    def propertyList: Array[String]
  }

  class SpeedDialSlot(propertySet: JSONObject) extends LinkAPIItem(propertySet) {
    println("Creating a new SpeedDialSlot")

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

    def createSpeedDial(position: Int,
                        properties: Map[String, String]): SpeedDialSlot = {
      val json = serverProxy.post("/rest/speeddial", properties)
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
      // val token = Http("https://auth.opera.com/service/oauth/request_token").param("oauth_callback","oob").oauth(consumer).asToken

      val token = Http("http://auth-test.opera.com/service/oauth/request_token").param("oauth_callback","oob").oauth(consumer).asToken

      println("Go to https://auth.opera.com/service/oauth/authorize?oauth_token=" + token.key)
      val verifier = Console.readLine("Enter verifier: ").trim

      accessToken = Http("https://auth.opera.com/service/oauth/access_token").oauth(consumer, token, verifier).asToken
      println("I got an access token! Namely, " + accessToken.key + " / " + accessToken.secret)
    }

    /* println(Http("https://link.api.opera.com/rest/speeddial/children/").header("content-type", "application/json").oauth(consumer, accessToken).asString) */
    println(Http("https://link.api.opera.com/rest/speeddial/children/").oauth(consumer, accessToken).asString)
  }
}
