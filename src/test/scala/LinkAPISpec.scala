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
    val properties = Map("title" -> "First Speed Dial creation example",
                         "uri"   -> "http://example.com")
    val speedDial = api.createSpeedDialSlot(1, properties)
    speedDial.title should equal("First Speed Dial creation example")
    speedDial.uri should equal("http://example.com")
  }

  it should "correctly update an existing Speed Dial slot" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "updateSpeedDialSlot-1")
    val properties = Map("title" -> "New title")
    val speedDial = api.updateSpeedDialSlot(1, properties)
    speedDial.title should equal("New title")
    speedDial.uri should equal("http://example.com")
  }

  it should "correctly delete an existing Speed Dial slot" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "deleteSpeedDialSlot-1")
    api.deleteSpeedDialSlot(1)
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
    val bookmarks = api.getBookmarksRecursively()
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

  it should "correctly recognise bookmark folders inside folders (you need to go deeper)" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmarks-3")
    val bookmarks = api.getBookmarksRecursively()
    bookmarks.length should equal(1)
    // The item is a folder
    val folder = bookmarks(0).asInstanceOf[BookmarkFolder]
    folder.id should equal("24387990426311DE8E95FBB28A4386E0")
    folder.itemType should equal("bookmark_folder")
    folder.title should equal("tmp")
    // Check the contents of the folder
    val children = folder.contents
    val entry1 = children(0).asInstanceOf[BookmarkFolder]
    val entry2 = children(1).asInstanceOf[Bookmark]
    entry1.id should equal("1CE31BB0342E11DFBF42FDB89FA75814")
    entry1.itemType should equal("bookmark_folder")
    entry1.title should equal("watch")
    // This subfolder has two bookmarks inside
    val grandchildren = entry1.contents
    val subitem1 = grandchildren(0).asInstanceOf[Bookmark]
    val subitem2 = grandchildren(1).asInstanceOf[Bookmark]
    subitem1.id should equal("8B817840591811E0A3139639F0EB548B")
    subitem1.itemType should equal("bookmark")
    subitem1.uri should equal("http://www.imdb.com/title/tt1341167/")
    subitem1.title should equal("Four Lions (2010) - IMDb")
    subitem2.id should equal("68C4ADC0564F11E0948BEA342F20E09C")
    subitem2.itemType should equal("bookmark")
    subitem2.uri should equal("http://www.weroy.org/watch.shtml")
    subitem2.title should equal("We. Arundhati Roy - Watch It")

    entry2.id should equal("772071F05A0B11E0A315CCF76087BBD9")
    entry2.itemType should equal("bookmark")
    entry2.title should equal("QWOP")
    entry2.uri should equal("http://www.foddy.net/Athletics.html")
  }

  it should "correctly recognise bookmark separators" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmarks-4")
    val bookmarks = api.getBookmarks()
    bookmarks.length should equal(1)
    val sep = bookmarks(0).asInstanceOf[BookmarkSeparator]
    sep.id should equal("26DFA4D09B9B11DDB3CCFAF9CF5DDB57")
    sep.itemType should equal("bookmark_separator")
  }

  it should "correctly ask for bookmarks inside a given folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmarks-5")
    val bookmarks = api.getBookmarks(Some("abc123"))
    bookmarks.length should equal(1)
    val sep = bookmarks(0).asInstanceOf[BookmarkSeparator]
    sep.id should equal("26DFA4D09B9B11DDB3CCFAF9CF5DDB57")
    sep.itemType should equal("bookmark_separator")

    val bookmarksRecursive = api.getBookmarksRecursively(Some("abc123"))
    bookmarksRecursive.length should equal(1)
    val sepRecursive = bookmarksRecursive(0).asInstanceOf[BookmarkSeparator]
    sepRecursive.id should equal("26DFA4D09B9B11DDB3CCFAF9CF5DDB57")
    sepRecursive.itemType should equal("bookmark_separator")
  }

  it should "correctly ask for a single bookmark" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmark-1")
    val bookmark = api.getBookmark("abc123").asInstanceOf[Bookmark]
    bookmark.id should equal("abc123")
    bookmark.title should equal("HCoder.org")
    bookmark.uri should equal("http://hcoder.org")
  }

  it should "correctly ask for a single bookmark folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmark-2")
    val bookmark_folder = api.getBookmark("abc456").asInstanceOf[BookmarkFolder]
    bookmark_folder.id should equal("abc456")
    bookmark_folder.title should equal("Blogs")
  }

  it should "correctly ask for a single bookmark separator" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmark-3")
    val bookmark_separator = api.getBookmark("abc789").asInstanceOf[BookmarkSeparator]
    bookmark_separator.id should equal("abc789")
  }

  it should "create a new bookmark properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createBookmark-1")
    val title    = "Title for the new bookmark"
    val uri      = "http://example.com"
    val nickname = "e"
    val bookmark = api.createBookmark(Map(
      "title"     -> title,
      "uri"       -> uri,
      "nickname"  -> nickname))
    bookmark.itemType should equal("bookmark")
    bookmark.title    should equal(title)
    bookmark.uri      should equal(uri)
    bookmark.nickname should equal(nickname)
  }

  it should "create a new bookmark folder properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createBookmarkFolder-1")
    val title    = "Folder title"
    val nickname = "e"
    val bookmarkFolder = api.createBookmarkFolder(Map(
      "title"     -> title,
      "nickname"  -> nickname))
    bookmarkFolder.itemType should equal("bookmark_folder")
    bookmarkFolder.title    should equal(title)
    bookmarkFolder.nickname should equal(nickname)
  }

  it should "create a new bookmark separator properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createBookmarkSeparator-1")
    val bookmarkSeparator = api.createBookmarkSeparator()
    bookmarkSeparator.id should not equal("")
  }

  it should "update a bookmark properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "updateBookmark-1")
    val newTitle = "Updated title"
    val bookmark = api.updateBookmark("123abc", Map("title" -> newTitle)).
                    asInstanceOf[Bookmark]
    bookmark.title should equal(newTitle)
  }

  it should "delete a bookmark properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "deleteBookmark-1")
    api.deleteBookmark("abc123")
  }

  it should "properly send a bookmark to trash" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "trashBookmark-1")
    val bookmark = api.trashBookmark("abc123").asInstanceOf[Bookmark]
    bookmark.id should equal("abc123")
    bookmark.title should not equal("")
  }

  it should "properly send a bookmark folder to trash" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "trashBookmark-2")
    val bookmarkFolder = api.trashBookmark("abc456").asInstanceOf[BookmarkFolder]
    bookmarkFolder.id should equal("abc456")
    bookmarkFolder.title should not equal("")
  }

  it should "properly move a bookmark inside a folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "moveBookmark-1")
    val bookmark = api.moveBookmarkInto("abc789",
                                        "def123").asInstanceOf[Bookmark]
    bookmark.id should equal("abc789")
    bookmark.title should not equal("")
  }

  it should "properly move a bookmark before another element" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "moveBookmark-2")
    val bookmark = api.moveBookmarkBefore("abc789",
                                          "def123").asInstanceOf[Bookmark]
    bookmark.id should equal("abc789")
    bookmark.title should not equal("")
  }

  it should "properly move a bookmark after another element" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "moveBookmark-3")
    val bookmark = api.moveBookmarkAfter("abc789",
                                         "def123").asInstanceOf[Bookmark]
    bookmark.id should equal("abc789")
    bookmark.title should not equal("")
  }

  it should "correctly ask for notes in the root folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNotes-1")
    val notes = api.getNotes()
    notes.length should equal(2)
    val firstNote = notes(0).asInstanceOf[Note]
    firstNote.id should equal("E22EE4E0524511E08C68A6DDBCB261C0")
    firstNote.content should equal("The Free Encyclopedia")
    firstNote.uri should equal("http://es.wikipedia.org/wiki/Wikipedia:Portada")
    val secondNote = notes(1).asInstanceOf[Note]
    secondNote.id should equal("E22F0BF0524511E08C69D46C240865CA")
    secondNote.content should equal("Email for Families and Individuals")
    secondNote.uri should equal("http://redir.opera.com/bookmarks/fastmail")
  }

  it should "correctly recognise notes inside folders" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNotes-2")
    val notes = api.getNotesRecursively()
    notes.length should equal(2)
    // First item is a folder
    val folder = notes(0).asInstanceOf[NoteFolder]
    folder.id should equal("E22DAC60524511E08C5DF05498BE0285")
    folder.itemType should equal("note_folder")
    folder.title should equal("Opera")
    // Check the contents of the folder
    val children = folder.contents
    val note1 = children(0).asInstanceOf[Note]
    val note2 = children(1).asInstanceOf[Note]
    note1.id should equal("E22DD370524511E08C5E9ABB8DF3792F")
    note1.itemType should equal("note")
    note1.content should equal("Download Opera")
    note1.uri should equal("")
    note2.id should equal("E22DFA80524511E08C5FCEAA6EDD412E")
    note2.itemType should equal("note")
    note2.content should equal("Latest news")
    note2.uri should equal("http://my.opera.com/")

    // Second item is a regular note
    val note = notes(1).asInstanceOf[Note]
    note.id should equal("E22E6FB0524511E08C63D6C3F6B25E7D")
    note.itemType should equal("note")
    note.content should equal("Some note created by hand")
    note.uri should equal("")
  }

  it should "correctly recognise note folders inside folders (you need to go deeper)" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNotes-3")
    val notes = api.getNotesRecursively()
    notes.length should equal(1)
    // The item is a folder
    val folder = notes(0).asInstanceOf[NoteFolder]
    folder.id should equal("24387990426311DE8E95FBB28A4386E0")
    folder.itemType should equal("note_folder")
    folder.title should equal("tmp")
    // Check the contents of the folder
    val children = folder.contents
    val entry1 = children(0).asInstanceOf[NoteFolder]
    val entry2 = children(1).asInstanceOf[Note]
    entry1.id should equal("1CE31BB0342E11DFBF42FDB89FA75814")
    entry1.itemType should equal("note_folder")
    entry1.title should equal("watch")
    // This subfolder has two notes inside
    val grandchildren = entry1.contents
    val subitem1 = grandchildren(0).asInstanceOf[Note]
    val subitem2 = grandchildren(1).asInstanceOf[Note]
    subitem1.id should equal("8B817840591811E0A3139639F0EB548B")
    subitem1.itemType should equal("note")
    subitem1.uri should equal("http://www.imdb.com/title/tt1341167/")
    subitem1.content should equal("Directed by Christopher Morris. With Will Adamsdale, Riz Ahmed, Adeel Akhtar, Kayvan Novak. 1")
    subitem2.id should equal("68C4ADC0564F11E0948BEA342F20E09C")
    subitem2.itemType should equal("note")
    subitem2.uri should equal("http://www.weroy.org/watch.shtml")
    subitem2.content should equal("We is a fast-paced 64 minute documentary that covers the world politics of power, war, corporations, deception and exploitation as seen through the eyes of Arundhati Roy.")

    entry2.id should equal("772071F05A0B11E0A315CCF76087BBD9")
    entry2.itemType should equal("note")
    entry2.content should equal("Athletics Game")
    entry2.uri should equal("http://www.foddy.net/Athletics.html")
  }

  it should "correctly recognise note separators" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNotes-4")
    val notes = api.getNotes()
    notes.length should equal(1)
    val sep = notes(0).asInstanceOf[NoteSeparator]
    sep.id should equal("26DFA4D09B9B11DDB3CCFAF9CF5DDB57")
    sep.itemType should equal("note_separator")
  }

  it should "correctly ask for notes inside a given folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNotes-5")
    val notes = api.getNotes(Some("abc123"))
    notes.length should equal(1)
    val sep = notes(0).asInstanceOf[NoteSeparator]
    sep.id should equal("26DFA4D09B9B11DDB3CCFAF9CF5DDB57")
    sep.itemType should equal("note_separator")

    val notesRecursive = api.getNotesRecursively(Some("abc123"))
    notesRecursive.length should equal(1)
    val sepRecursive = notesRecursive(0).asInstanceOf[NoteSeparator]
    sepRecursive.id should equal("26DFA4D09B9B11DDB3CCFAF9CF5DDB57")
    sepRecursive.itemType should equal("note_separator")
  }

  it should "correctly ask for a single note" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNote-1")
    val note = api.getNote("abc123").asInstanceOf[Note]
    note.id should equal("abc123")
    note.content should equal("HCoder.org")
    note.uri should equal("http://hcoder.org")
  }

  it should "correctly ask for a single note folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNote-2")
    val note_folder = api.getNote("abc456").asInstanceOf[NoteFolder]
    note_folder.id should equal("abc456")
    note_folder.title should equal("Blogs")
  }

  it should "correctly ask for a single note separator" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNote-3")
    val note_separator = api.getNote("abc789").asInstanceOf[NoteSeparator]
    note_separator.id should equal("abc789")
  }

  it should "create a new note properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createNote-1")
    val title    = "Content for the new note"
    val uri      = "http://example.com"
    val note = api.createNote(Map(
      "content"   -> title,
      "uri"       -> uri))
    note.itemType should equal("note")
    note.content  should equal(title)
    note.uri      should equal(uri)
  }

  it should "create a new note folder properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createNoteFolder-1")
    val title    = "Folder title"
    val noteFolder = api.createNoteFolder(Map(
      "title"     -> title))
    noteFolder.itemType should equal("note_folder")
    noteFolder.title    should equal(title)
  }

  it should "create a new note separator properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createNoteSeparator-1")
    val noteSeparator = api.createNoteSeparator()
    noteSeparator.id should not equal("")
  }

  it should "update a note properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "updateNote-1")
    val newTitle = "Updated content"
    val note = api.updateNote("123abc", Map("content" -> newTitle)).
                    asInstanceOf[Note]
    note.content should equal(newTitle)
  }

  it should "delete a note properly" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "deleteNote-1")
    api.deleteNote("abc123")
  }

  it should "properly send a note to trash" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "trashNote-1")
    val note = api.trashNote("abc123").asInstanceOf[Note]
    note.id should equal("abc123")
    note.content should not equal("")
  }

  it should "properly send a note folder to trash" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "trashNote-2")
    val noteFolder = api.trashNote("abc456").asInstanceOf[NoteFolder]
    noteFolder.id should equal("abc456")
    noteFolder.title should not equal("")
  }

  it should "properly move a note inside a folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "moveNote-1")
    val note = api.moveNoteInto("abc789", "def123").asInstanceOf[Note]
    note.id should equal("abc789")
    note.content should not equal("")
  }

  it should "properly move a note before another element" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "moveNote-2")
    val note = api.moveNoteBefore("abc789", "def123").asInstanceOf[Note]
    note.id should equal("abc789")
    note.content should not equal("")
  }

  it should "properly move a note after another element" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "moveNote-3")
    val note = api.moveNoteAfter("abc789", "def123").asInstanceOf[Note]
    note.id should equal("abc789")
    note.content should not equal("")
  }

  it should "properly get the list of urlfilters" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getUrlFilters-1")
    val filters = api.getUrlFilters()
    filters.length should equal(3)
    filters(0).id should equal("abc123")
    filters(0).content should equal("http://ads.example.com/*")
    filters(0).filterType should equal("exclude")
    filters(1).id should equal("abc456")
    filters(1).content should equal("http://www.evilcorp.com/ads/*")
    filters(1).filterType should equal("exclude")
    filters(2).id should equal("abc789")
    filters(2).content should equal("*")
    filters(2).filterType should equal("include")
  }

  it should "properly get a urlfilter" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getUrlFilter-1")
    val filter = api.getUrlFilter("abc123")
    filter.id should equal("abc123")
    filter.content should equal("http://ads.example.com/*")
    filter.filterType should equal("exclude")
  }

  it should "properly update a urlfilter" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "updateUrlFilter-1")
    val filter = api.updateUrlFilter("abc123",
                                     Map("content" ->
                                           "http://*.evilcorp.com/*"))
    filter.id should equal("abc123")
    filter.content should equal("http://*.evilcorp.com/*")
    filter.filterType should equal("exclude")
  }

  it should "properly delete a urlfilter" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "deleteUrlFilter-1")
    api.deleteUrlFilter("abc123")
  }
}
