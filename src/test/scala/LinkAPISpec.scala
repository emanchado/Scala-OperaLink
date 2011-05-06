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
    speedDials(0).reloadInterval should equal(2147483646)
    speedDials(0).reloadEnabled should equal(false)
    speedDials(0).reloadOnlyIfExpired should equal(false)

    speedDials(1).id should equal("3")
    speedDials(1).uri should equal("http://redir.opera.com/speeddials/myopera/")
    speedDials(1).title should equal("My Opera")
    speedDials(1).reloadInterval should equal(3600)
    speedDials(1).reloadEnabled should equal(true)
    speedDials(1).reloadOnlyIfExpired should equal(true)
  }

  it should "correctly ask for a single Speed Dial slot" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getSpeedDialSlot-1")
    val speedDial = api.getSpeedDialSlot(1)

    speedDial.id should equal("1")
    speedDial.uri should equal("http://redir.opera.com/speeddials/portal/")
    speedDial.title should equal("Opera Portal beta")
    speedDial.reloadEnabled should equal(false)
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
    bookmark.created should equal("2011-03-19T16:28:12Z")
    bookmark.visited should equal("2011-04-14T12:03:27Z")
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

  it should "correctly tell apart target folders from regular bookmark folders" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmark-4")
    val folders = api.getBookmarks().asInstanceOf[Seq[BookmarkFolder]]
    folders(0).id should equal("abc123")
    folders(0).target should equal("mini")
    folders(0).isTargetFolder should equal(true)
    folders(1).id should equal("abc456")
    folders(1).target should equal("")
    folders(1).isTargetFolder should equal(false)

    val targetBookmarkFolder = api.getBookmark("abc123").asInstanceOf[BookmarkFolder]
    targetBookmarkFolder.id should equal("abc123")
    targetBookmarkFolder.target should equal("mini")

    val bookmarkFolder = api.getBookmark("abc456").asInstanceOf[BookmarkFolder]
    bookmarkFolder.id should equal("abc456")
    bookmarkFolder.target should equal("")
  }

  it should "correctly tell apart trash folders from regular bookmark folders" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmark-5")
    val folders = api.getBookmarks().asInstanceOf[Seq[BookmarkFolder]]
    folders(0).id should equal("abc123")
    folders(0).folderType should equal("")
    folders(0).isTrashFolder should equal(false)
    folders(1).id should equal("abc456")
    folders(1).folderType should equal("trash")
    folders(1).isTrashFolder should equal(true)

    val bookmarkFolder = api.getBookmark("abc123").asInstanceOf[BookmarkFolder]
    bookmarkFolder.id should equal("abc123")
    bookmarkFolder.folderType should equal("")
    bookmarkFolder.isTrashFolder should equal(false)

    val trashFolder = api.getBookmark("abc456").asInstanceOf[BookmarkFolder]
    trashFolder.id should equal("abc456")
    trashFolder.folderType should equal("trash")
    trashFolder.isTrashFolder should equal(true)
  }

  it should "correctly recognise an empty folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getBookmark-6")
    val folder = api.getBookmark("E7355430508111E087A8D234149BB1CF").asInstanceOf[BookmarkFolder]
    folder.id should equal("E7355430508111E087A8D234149BB1CF")
    folder.title should equal("Opera")
    folder.folderType should equal("")
    folder.isTrashFolder should equal(false)
    folder.contents.length should equal(0)
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

  it should "correctly recognise an empty note folder" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getNote-4")
    val note_folder = api.getNote("abc789").asInstanceOf[NoteFolder]
    note_folder.id should equal("abc789")
    note_folder.contents.length should equal(0)
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

  it should "properly create a urlfilter" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createUrlFilter-1")
    val properties = Map("type"    -> "exclude",
                         "content" -> "http://ads.example.com/*")
    val urlFilter = api.createUrlFilter(properties)
    urlFilter.content should equal(properties("content"))
    urlFilter.filterType should equal(properties("type"))
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

  it should "properly get all search engines" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getSearchEngines-1")
    val engines = api.getSearchEngines()
    engines.length should equal(2)
    engines(0).id should equal("438F1230685A11E09B6BB5FEF7932261")
    engines(0).title should equal("Uncyclopedia")
    engines(0).uri should equal("http://uncyclopedia.wikia.com/index.php?title=Special%3ASearch&search=%s&go=Go")
    engines(0).isPost should equal(false)
    engines(0).key should equal("u")
    engines(0).showInPersonalBar should equal(false)
    engines(0).rawIcon should equal("")
    engines(1).id should equal("1E4C9490DA2311DFB66C91ED324A295A")
    engines(1).title should equal("EmacsWiki")
    engines(1).uri should equal("http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=%s&sa=Search&siteurl=emacswiki.org%2F")
    engines(1).isPost should equal(false)
    engines(1).key should equal("ew")
    engines(1).showInPersonalBar should equal(false)
    val iconBytes = Array(-119, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 16, 0, 0, 0, 16, 8, 6, 0, 0, 0, 31, -13, -1, 97, 0, 0, 0, 82, 73, 68, 65, 84, 120, -38, 99, 96, 24, 5, -40, -64, 127, 2, -104, 56, 13, -65, -10, 72, -96, 96, 66, 6, -126, 57, -56, 0, -39, 16, 108, 114, -24, -82, -127, 75, 46, 83, 100, 0, 99, 100, -123, 48, 16, 49, -59, 9, -116, -111, -28, 80, 13, -128, 105, -58, 102, 0, 76, 51, -51, 12, 96, -64, -29, 79, 92, 97, 64, 48, 10, 25, 112, -80, -1, -109, -102, 38, -122, 59, 0, 0, 38, 65, -52, -54, -75, -117, 36, -102, 0, 0, 0, 0, 73, 69, 78, 68, -82, 66, 96, -126)
    engines(1).icon should equal(iconBytes)
    engines(1).rawIcon should equal("iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAUklEQVR42mNgGAXYwH8CmDgNv/ZIoGBCBoI5yADZEGxy6K6BSy5TZABjZIUwEDHFCYyR5FANgGnGZgBMM80MYMDjT1xhQDAKGXCw/5OaJoY7AAAmQczKtYskmgAAAABJRU5ErkJggg==")
  }

  it should "properly get one search engine" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getSearchEngine-1")
    val engine = api.getSearchEngine("438F1230685A11E09B6BB5FEF7932261")
    engine.id should equal("438F1230685A11E09B6BB5FEF7932261")
    engine.title should equal("Uncyclopedia")
    engine.uri should equal("http://uncyclopedia.wikia.com/index.php?title=Special%3ASearch&search=%s&go=Go")
    engine.isPost should equal(false)
    engine.key should equal("u")
    engine.showInPersonalBar should equal(false)
    engine.encoding should equal("utf-8")
  }

  it should "properly recognise boolean fields" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getSearchEngine-2")
    val engine = api.getSearchEngine("CB877760C0C411DF8F9AC996B55E7F5D")
    engine.id should equal("CB877760C0C411DF8F9AC996B55E7F5D")
    engine.isPost should equal(true)
    engine.showInPersonalBar should equal(false)

    val engine2 = api.getSearchEngine("438F1230685A11E09B6BB5FEF7932261")
    engine2.id should equal("438F1230685A11E09B6BB5FEF7932261")
    engine2.isPost should equal(false)
    engine2.showInPersonalBar should equal(true)
  }

  it should "properly split form parameters" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "getSearchEngine-3")
    val engine = api.getSearchEngine("D67C6EC0901B11DFA617C412E565720D")
    engine.id should equal("D67C6EC0901B11DFA617C412E565720D")
    engine.isPost should equal(true)
    engine.baseUri should equal("http://www.wikidioms.com/")
    engine.params should equal(Map("search_keywords" -> "%s",
                                   "search_within"   -> "0",
                                   "op"              -> "Search",
                                   "form_build_id"   ->
                                     "form-1acd210b990803618c2ee090bc288417",
                                   "form_id"         -> "get_search_form"))

    val engine2 = api.getSearchEngine("438F1230685A11E09B6BB5FEF7932261")
    engine2.id should equal("438F1230685A11E09B6BB5FEF7932261")
    engine2.isPost should equal(false)
    engine2.baseUri should equal("http://uncyclopedia.wikia.com/index.php")
    engine2.params should equal(Map("title"  -> "Special%3ASearch",
                                    "search" -> "%s",
                                    "go"     -> "Go"))

    val engine3 = api.getSearchEngine("826799903DFE11E0A4D5D31252A963B4")
    engine3.id should equal("826799903DFE11E0A4D5D31252A963B4")
    engine3.isPost should equal(false)
    engine3.baseUri should equal("http://dictionary.reverso.net/english-spanish/%s")
    engine3.params should equal(Map[String, String]())
  }

  it should "properly create a search engine" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "createSearchEngine-1")
    val properties = Map("title"    -> "Uncyclopedia",
                         "key"      -> "u",
                         "uri"      -> "http://uncyclopedia.wikia.com/index.php?title=Special%3ASearch&search=%s&go=Go",
                         "encoding" -> "utf-8")
    val engine = api.createSearchEngine(properties)
    engine.id should not equal("")
    engine.title should equal(properties("title"))
    engine.uri should equal(properties("uri"))
    engine.key should equal(properties("key"))
    engine.isPost should equal(false)
    engine.showInPersonalBar should equal(false)
  }

  it should "properly update a search engine" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "updateSearchEngine-1")
    val engine = api.updateSearchEngine("438F1230685A11E09B6BB5FEF7932261",
                                        Map("title" -> "Uncyclopedia"))
    engine.id should equal("438F1230685A11E09B6BB5FEF7932261")
    engine.title should equal("Uncyclopedia")
    engine.uri should equal("http://uncyclopedia.wikia.com/index.php?title=Special%3ASearch&search=%s&go=Go")
    engine.isPost should equal(false)
    engine.key should equal("u")
    engine.showInPersonalBar should equal(false)
  }

  it should "properly delete a search engine" in {
    api.serverProxy = new TestLinkServerProxy(fakeConsumer,
                                              fakeAccessToken,
                                              "deleteSearchEngine-1")
    api.deleteSearchEngine("438F1230685A11E09B6BB5FEF7932261")
  }
}
