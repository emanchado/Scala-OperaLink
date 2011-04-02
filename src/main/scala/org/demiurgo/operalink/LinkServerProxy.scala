import scalaj.http.{Http, Token}

package org.demiurgo.operalink {
  class LinkServerProxy(var consumer: Token,
                        var accessToken: Token) {
    val serverBaseUrl = "https://link.api.opera.com"

    def get(path: String): String = {
      return Http(serverBaseUrl + path).oauth(consumer, accessToken).asString
    }

    def post(path: String, data: String): String = {
      return Http.postData(serverBaseUrl + path, data).
                oauth(consumer, accessToken).asString
    }

    def post(path: String, params: Map[String, String]): String = {
      var request = Http.post(serverBaseUrl + path)
      println("Making a request to path " + serverBaseUrl + path)
      for ((key,value) <- params) {
        println("Adding " + key + "=" + value + " as a parameter")
        request = request.param(key, value)
      }
      return request.oauth(consumer, accessToken).asString
    }
  }
}
