import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaj.http.{Http, Token}

import org.demiurgo.operalink._

class LinkAPISpec extends FlatSpec with ShouldMatchers {
  val fakeConsumer    = Token("foo", "bar")
  val fakeAccessToken = Token("foo", "bar")
  val proxy = new TestLinkServerProxy(fakeConsumer,
                                      fakeAccessToken)
  val api = new LinkAPI(fakeConsumer, fakeAccessToken)
  api.serverProxy = proxy

  "The API object" should "correctly convert a Speed Dial slot" in {
    val speedDials = api.getSpeedDial
    speedDials.length should equal(1)

    speedDials(0).uri should equal("http://redir.opera.com/speeddials/portal/")
    speedDials(0).title should equal("Opera Portal beta")
    speedDials(0).reload_enabled should equal("0")
  }
}
