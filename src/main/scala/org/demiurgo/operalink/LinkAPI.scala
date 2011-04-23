import scalaj.http.{Http, Token}
import scala.util.parsing.json.{JSON, JSONObject, JSONArray}
import org.apache.commons.codec.binary.Base64
import org.demiurgo.operalink.LinkServerProxy

package org.demiurgo.operalink {
  abstract class LinkAPIItem(propertySet: JSONObject) {
    val propertyHash = propertySet.obj("properties").asInstanceOf[JSONObject].obj.asInstanceOf[Map[String, String]]
    val baseHash = propertySet.obj.asInstanceOf[Map[String, String]]

    def id: String = baseHash("id")
    def itemType: String = baseHash("item_type")
    def propertyList: Array[String]
  }

  object LinkAPIItem {
    def fromJsonObject(jsonObject: JSONObject): LinkAPIItem = {
      return jsonObject.obj.asInstanceOf[Map[String, String]]("item_type") match {
        case "speeddial" => new SpeedDialSlot(jsonObject)
        case "bookmark" => new Bookmark(jsonObject)
        case "bookmark_folder" => new BookmarkFolder(jsonObject)
        case "bookmark_separator" => new BookmarkSeparator(jsonObject)
        case "note" => new Note(jsonObject)
        case "note_folder" => new NoteFolder(jsonObject)
        case "note_separator" => new NoteSeparator(jsonObject)
        case "urlfilter" => new UrlFilter(jsonObject)
        case "search_engine" => new SearchEngine(jsonObject)
      }
    }
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


  abstract class BookmarkEntry(propertySet: JSONObject) extends LinkAPIItem(propertySet) {
  }


  class Bookmark(propertySet: JSONObject) extends BookmarkEntry(propertySet) {
    def propertyList: Array[String] = {
      return Array("uri", "title", "description", "nickname",
                   "icon", "created", "visited")
    }

    def title: String = propertyHash("title")
    def uri: String = propertyHash("uri")
    def description: String = propertyHash("description")
    def nickname: String = propertyHash("nickname")
    def icon: String = propertyHash("icon")
  }


  class BookmarkFolder(propertySet: JSONObject) extends BookmarkEntry(propertySet) {
    val childrenList = propertySet.obj("children").asInstanceOf[JSONArray].list.asInstanceOf[Seq[JSONObject]]
    def propertyList: Array[String] = {
      return Array("title", "description", "nickname",
                   "type", "target")
    }

    def title: String = propertyHash("title")
    def nickname: String = propertyHash("nickname")
    def target: String = propertyHash.getOrElse("target", "")
    def isTargetFolder: Boolean = if (propertyHash.getOrElse("target", "") == "") false else true
    def contents: Seq[BookmarkEntry] = {
      return for { entry <- childrenList }
                 yield LinkAPIItem.fromJsonObject(entry).asInstanceOf[BookmarkEntry]
    }
  }


  class BookmarkSeparator(propertySet: JSONObject) extends BookmarkEntry(propertySet) {
    def propertyList: Array[String] = {
      return Array()
    }
  }


  abstract class NoteEntry(propertySet: JSONObject) extends LinkAPIItem(propertySet) {
  }


  class Note(propertySet: JSONObject) extends NoteEntry(propertySet) {
    def propertyList: Array[String] = {
      return Array("uri", "content", "created")
    }

    def content: String = propertyHash("content")
    def uri: String = propertyHash.getOrElse("uri", "")
    def created: String = propertyHash("created")
  }


  class NoteFolder(propertySet: JSONObject) extends NoteEntry(propertySet) {
    val childrenList = propertySet.obj("children").asInstanceOf[JSONArray].list.asInstanceOf[Seq[JSONObject]]
    def propertyList: Array[String] = {
      return Array("title", "description", "nickname",
                   "type", "target")
    }

    def title: String = propertyHash("title")
    def contents: Seq[NoteEntry] = {
      return for { entry <- childrenList }
                 yield LinkAPIItem.fromJsonObject(entry).asInstanceOf[NoteEntry]
    }
  }


  class NoteSeparator(propertySet: JSONObject) extends NoteEntry(propertySet) {
    def propertyList: Array[String] = {
      return Array()
    }
  }


  class UrlFilter(propertySet: JSONObject) extends LinkAPIItem(propertySet) {
    def propertyList: Array[String] = {
      return Array("content", "type")
    }

    def content: String = propertyHash("content")
    def filterType: String = propertyHash("type")
  }


  class SearchEngine(propertySet: JSONObject) extends LinkAPIItem(propertySet) {
    def propertyList: Array[String] = {
      return Array("title", "key", "icon")
    }

    def title: String = propertyHash("title")
    def uri: String = propertyHash("uri")
    def key: String = propertyHash("key")
    def isPost: Boolean = propertyHash("is_post").asInstanceOf[Boolean]
    def postQuery: String = propertyHash("post_query")
    def showInPersonalBar: Boolean = propertyHash("show_in_personal_bar").asInstanceOf[Boolean]
    def icon: Array[Byte] = Base64.decodeBase64(propertyHash("icon"))
  }


  class LinkAPI(val consumer: Token, val accessToken: Token) {
    var serverProxy = new LinkServerProxy(consumer, accessToken)

    def genericGetRequest(dataType: String,
                          apiMethod: String,
                          itemId: Option[String] = None): Seq[LinkAPIItem] = {
      var itemIdString = itemId match {
        case Some(id) => id + "/"
        case None     => ""
      }
      val jsonText =
        serverProxy.get("/rest/" + dataType + "/" + itemIdString + apiMethod)
      return for { item <- JSON.parseRaw(jsonText).get.asInstanceOf[JSONArray].list }
             yield LinkAPIItem.fromJsonObject(item.asInstanceOf[JSONObject])
    }

    def genericPostRequest(dataType: String,
                           properties: Map[String, String],
                           itemId: Option[String] = None): Seq[LinkAPIItem] = {
      var itemIdString = itemId match {
        case Some(id) => id
        case None     => ""
      }
      val json = serverProxy.post("/rest/" + dataType +
                                    "/" + itemIdString,
                                  properties)
      return for { item <- JSON.parseRaw(json).get.asInstanceOf[JSONArray].list }
             yield LinkAPIItem.fromJsonObject(item.asInstanceOf[JSONObject])
    }

    def getSpeedDial: Seq[SpeedDialSlot] = {
      return genericGetRequest("speeddial", "children").asInstanceOf[Seq[SpeedDialSlot]]
    }

    def getSpeedDialSlot(position: Int): SpeedDialSlot = {
      return genericGetRequest("speeddial", "", Some(position.toString)).asInstanceOf[Seq[SpeedDialSlot]](0)
    }

    def createSpeedDialSlot(position: Int,
                            properties: Map[String, String]): SpeedDialSlot = {
      return genericPostRequest("speeddial", properties,
                                Some(position.toString))(0).
              asInstanceOf[SpeedDialSlot]
    }

    def updateSpeedDialSlot(position: Int,
                            properties: Map[String, String]): SpeedDialSlot = {
      return genericPostRequest("speeddial",
                                properties ++ Map("api_method" -> "update"),
                                Some(position.toString))(0).
              asInstanceOf[SpeedDialSlot]
    }

    def deleteSpeedDialSlot(position: Int) {
      val response =
        serverProxy.post("/rest/speeddial/" + position,
                         Map("api_method" -> "delete"))
      if (response != "") {
        throw new Exception("Error deleting Speed Dial slot " + position)
      }
    }

    def getBookmarks(fromFolder: Option[String] = None): Seq[BookmarkEntry] = {
      return genericGetRequest("bookmark", "children", fromFolder).asInstanceOf[Seq[BookmarkEntry]]
    }

    def getBookmarksRecursively(fromFolder: Option[String] = None): Seq[BookmarkEntry] = {
      return genericGetRequest("bookmark", "descendants", fromFolder).asInstanceOf[Seq[BookmarkEntry]]
    }

    def getBookmark(id: String): BookmarkEntry = {
      return genericGetRequest("bookmark", "", Some(id)).asInstanceOf[Seq[BookmarkEntry]](0)
    }

    def createBookmarkGeneric(properties: Map[String, String]): BookmarkEntry = {
      return genericPostRequest("bookmark", properties, None)(0).
              asInstanceOf[BookmarkEntry]
    }

    def createBookmark(properties: Map[String, String]): Bookmark = {
      return createBookmarkGeneric(properties ++
                                     Map("item_type" -> "bookmark")).
              asInstanceOf[Bookmark]
    }

    def createBookmarkFolder(properties: Map[String, String]): BookmarkFolder = {
      return createBookmarkGeneric(properties ++
                                     Map("item_type" -> "bookmark_folder")).
              asInstanceOf[BookmarkFolder]
    }

    def createBookmarkSeparator(): BookmarkSeparator = {
      return createBookmarkGeneric(Map("item_type" -> "bookmark_separator")).
              asInstanceOf[BookmarkSeparator]
    }

    def updateBookmark(id: String,
                       properties: Map[String, String]): BookmarkEntry = {
      return genericPostRequest("bookmark",
                                properties ++ Map("api_method" -> "update"),
                                Some(id))(0).
              asInstanceOf[BookmarkEntry]
    }

    def deleteBookmark(id: String) {
      val response =
        serverProxy.post("/rest/bookmark/" + id, Map("api_method" -> "delete"))
      if (response != "") {
        throw new Exception("Error deleting bookmark " + id)
      }
    }

    def trashBookmark(id: String): BookmarkEntry = {
      return genericPostRequest("bookmark", Map("api_method" -> "trash"),
                                Some(id))(0).
              asInstanceOf[BookmarkEntry]
    }

    def moveBookmarkGeneric(id: String, folderId: String,
                            relativePosition: String): BookmarkEntry = {
      return genericPostRequest("bookmark",
                                Map("api_method" -> "move",
                                    "relative_position" -> relativePosition,
                                    "reference_item" -> folderId),
                                Some(id))(0).
              asInstanceOf[BookmarkEntry]
    }

    def moveBookmarkInto(id: String, folderId: String): BookmarkEntry = {
      return moveBookmarkGeneric(id, folderId, "into")
    }

    def moveBookmarkBefore(id: String, folderId: String): BookmarkEntry = {
      return moveBookmarkGeneric(id, folderId, "before")
    }

    def moveBookmarkAfter(id: String, folderId: String): BookmarkEntry = {
      return moveBookmarkGeneric(id, folderId, "after")
    }

    def getNotes(fromFolder: Option[String] = None): Seq[NoteEntry] = {
      return genericGetRequest("note", "children", fromFolder).asInstanceOf[Seq[NoteEntry]]
    }

    def getNotesRecursively(fromFolder: Option[String] = None): Seq[NoteEntry] = {
      return genericGetRequest("note", "descendants", fromFolder).asInstanceOf[Seq[NoteEntry]]
    }

    def getNote(id: String): NoteEntry = {
      return genericGetRequest("note", "", Some(id)).
              asInstanceOf[Seq[NoteEntry]](0)
    }

    def createNoteGeneric(properties: Map[String, String]): NoteEntry = {
      return genericPostRequest("note", properties, None)(0).
              asInstanceOf[NoteEntry]
    }

    def createNote(properties: Map[String, String]): Note = {
      return createNoteGeneric(properties ++
                                     Map("item_type" -> "note")).
              asInstanceOf[Note]
    }

    def createNoteFolder(properties: Map[String, String]): NoteFolder = {
      return createNoteGeneric(properties ++
                                     Map("item_type" -> "note_folder")).
              asInstanceOf[NoteFolder]
    }

    def createNoteSeparator(): NoteSeparator = {
      return createNoteGeneric(Map("item_type" -> "note_separator")).
              asInstanceOf[NoteSeparator]
    }

    def updateNote(id: String,
                       properties: Map[String, String]): NoteEntry = {
      return genericPostRequest("note",
                                properties ++ Map("api_method" -> "update"),
                                Some(id))(0).
              asInstanceOf[NoteEntry]
    }

    def deleteNote(id: String) {
      val response =
        serverProxy.post("/rest/note/" + id, Map("api_method" -> "delete"))
      if (response != "") {
        throw new Exception("Error deleting note " + id)
      }
    }

    def trashNote(id: String): NoteEntry = {
      return genericPostRequest("note", Map("api_method" -> "trash"),
                                Some(id))(0).
              asInstanceOf[NoteEntry]
    }

    def moveNoteGeneric(id: String, folderId: String,
                            relativePosition: String): NoteEntry = {
      return genericPostRequest("note",
                                Map("api_method" -> "move",
                                    "relative_position" -> relativePosition,
                                    "reference_item" -> folderId),
                                Some(id))(0).
              asInstanceOf[NoteEntry]
    }

    def moveNoteInto(id: String, folderId: String): NoteEntry = {
      return moveNoteGeneric(id, folderId, "into")
    }

    def moveNoteBefore(id: String, folderId: String): NoteEntry = {
      return moveNoteGeneric(id, folderId, "before")
    }

    def moveNoteAfter(id: String, folderId: String): NoteEntry = {
      return moveNoteGeneric(id, folderId, "after")
    }

    def getUrlFilters(): Seq[UrlFilter] = {
      return genericGetRequest("urlfilter", "children").
              asInstanceOf[Seq[UrlFilter]]
    }

    def getUrlFilter(id: String): UrlFilter = {
      return genericGetRequest("urlfilter", "", Some(id))(0).
              asInstanceOf[UrlFilter]
    }

    def updateUrlFilter(id: String,
                        properties: Map[String, String]): UrlFilter = {
      return genericPostRequest("urlfilter",
                                properties ++ Map("api_method" -> "update"),
                                Some(id))(0).
              asInstanceOf[UrlFilter]
    }

    def deleteUrlFilter(id: String) {
      val response =
        serverProxy.post("/rest/urlfilter/" + id, Map("api_method" -> "delete"))
      if (response != "") {
        throw new Exception("Error deleting urlfilter " + id)
      }
    }

    def getSearchEngines(): Seq[SearchEngine] = {
      return genericGetRequest("search_engine", "children").
              asInstanceOf[Seq[SearchEngine]]
    }

    def getSearchEngine(id: String): SearchEngine = {
      return genericGetRequest("search_engine", "", Some(id))(0).
              asInstanceOf[SearchEngine]
    }

    def updateSearchEngine(id: String,
                           properties: Map[String, String]): SearchEngine = {
      return genericPostRequest("search_engine",
                                properties ++ Map("api_method" -> "update"),
                                Some(id))(0).
              asInstanceOf[SearchEngine]
    }

    def deleteSearchEngine(id: String) {
      val response =
        serverProxy.post("/rest/search_engine/" + id,
                         Map("api_method" -> "delete"))
      if (response != "") {
        throw new Exception("Error deleting search engine " + id)
      }
    }
  }
}
