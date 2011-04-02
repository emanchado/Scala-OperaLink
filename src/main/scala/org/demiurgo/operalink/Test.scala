import scalaj.http.{Http, Token, HttpException}
import scala.collection.mutable.HashMap
import scala.io
import scala.util.parsing.json.{JSON, JSONArray, JSONObject}

package org.demiurgo.operalink {
  object Test {
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

      val api = new LinkAPI(consumer, accessToken)
      val speedDial = api.getSpeedDial
      for (slot <- speedDial) {
        println(slot.position + " - " + slot.uri + " (" + slot.title + ")")
      }

      api.updateSpeedDialSlot(6, Map[String, String]("title" -> "New title"))
    }
  }
}
