import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaj.http.{Http, Token}
import scala.util.parsing.json.JSON
import org.demiurgo.operalink._

class LinkAPISpec extends FlatSpec with ShouldMatchers {
  val fakeConsumer    = Token("foo", "bar")
  val fakeAccessToken = Token("foo", "bar")
  val api = new LinkAPI(fakeConsumer, fakeAccessToken)

  "The API object" should "correctly get all Speed Dials" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getSpeedDial-1")
    val speedDials = api.getSpeedDial
    speedDials.length should equal(2)

    speedDials(0).id should equal("1")
    speedDials(0).uri should equal("http://redir.opera.com/speeddials/portal/")
    speedDials(0).title should equal("Opera Portal beta")
    speedDials(0).reloadEnabled should equal("0")

    speedDials(1).id should equal("3")
    speedDials(1).uri should equal("http://redir.opera.com/speeddials/myopera/")
    speedDials(1).title should equal("My Opera")
    speedDials(1).reloadEnabled should equal("0")
  }

  it should "correctly ask for a single Speed Dial slot" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getSpeedDialSlot-1")
    val speedDial = api.getSpeedDialSlot(1)

    speedDial.id should equal("1")
    speedDial.uri should equal("http://redir.opera.com/speeddials/portal/")
    speedDial.title should equal("Opera Portal beta")
    speedDial.reloadEnabled should equal("0")
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

  it should "correctly update an existing Speed Dial slot" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "updateSpeedDialSlot-1")
    val properties = Map[String, String]("title" -> "New title")
    val speedDial = api.updateSpeedDialSlot(1, properties)
    speedDial.title should equal("New title")
    speedDial.uri should equal("http://example.com")
  }

  it should "correctly ask for bookmarks in the root folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmarks-1")
    val bookmarks = api.getBookmarks()
    bookmarks.length should equal(2)
    val firstBookmark = bookmarks(0).asInstanceOf[Bookmark]
    firstBookmark.id should equal("E22EE4E0524511E08C68A6DDBCB261C0")
    firstBookmark.title should equal("Wikipedia")
    firstBookmark.uri should equal("http://es.wikipedia.org/wiki/Wikipedia:Portada")
    val secondBookmark = bookmarks(1).asInstanceOf[Bookmark]
    secondBookmark.id should equal("E22F0BF0524511E08C69D46C240865CA")
    secondBookmark.title should equal("FastMail")
    secondBookmark.uri should equal("http://redir.opera.com/bookmarks/fastmail")
  }

  it should "correctly recognise bookmarks inside folders" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmarks-2")
    val bookmarks = api.getBookmarks(Map[String, String]("recursive" -> "true"))
    bookmarks.length should equal(2)
    // First item is a folder
    val folder = bookmarks(0).asInstanceOf[BookmarkFolder]
    folder.id should equal("E22DAC60524511E08C5DF05498BE0285")
    folder.itemType should equal("bookmark_folder")
    folder.title should equal("Opera")
    // Check the contents of the folder
    val children = folder.contents
    val bm1 = children(0).asInstanceOf[Bookmark]
    val bm2 = children(1).asInstanceOf[Bookmark]
    bm1.id should equal("E22DD370524511E08C5E9ABB8DF3792F")
    bm1.itemType should equal("bookmark")
    bm1.title should equal("Download Opera")
    bm1.uri should equal("http://www.opera.com/download/")
    bm2.id should equal("E22DFA80524511E08C5FCEAA6EDD412E")
    bm2.itemType should equal("bookmark")
    bm2.title should equal("My Opera Community")
    bm2.uri should equal("http://my.opera.com/")

    // Second item is a regular bookmark
    val bookmark = bookmarks(1).asInstanceOf[Bookmark]
    bookmark.id should equal("E22E6FB0524511E08C63D6C3F6B25E7D")
    bookmark.itemType should equal("bookmark")
    bookmark.title should equal("Kayak")
    bookmark.uri should equal("http://redir.opera.com/bookmarks/kayak")
  }
}
