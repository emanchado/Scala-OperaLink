import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaj.http.{Http, Token}

import org.demiurgo.operalink._

class LinkAPISpec extends FlatSpec with ShouldMatchers {
  val fakeConsumer    = Token("foo", "bar")
  val fakeAccessToken = Token("foo", "bar")
  val api = new LinkAPI(fakeConsumer, fakeAccessToken)

  "The API object" should "correctly convert a Speed Dial slot" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getSpeedDial-1")
    val speedDials = api.getSpeedDial
    speedDials.length should equal(1)

    speedDials(0).uri should equal("http://redir.opera.com/speeddials/portal/")
    speedDials(0).title should equal("Opera Portal beta")
    speedDials(0).reloadEnabled should equal("0")
  }

  it should "correctly create a new Speed Dial" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createSpeedDial-1")
    val properties = Map[String, String](
      "title" -> "First Speed Dial creation example",
      "uri"   -> "http://example.com")
    val speedDial = api.createSpeedDial(1, properties)
    speedDial.title should equal("First Speed Dial creation example")
    speedDial.uri should equal("http://example.com")
  }
}
