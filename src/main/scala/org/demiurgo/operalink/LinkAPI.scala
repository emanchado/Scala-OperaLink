import scalaj.http.{Http, Token}
import scala.util.parsing.json.{JSON, JSONObject, JSONArray}
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
        case "bookmark" => new Bookmark(jsonObject)
        case "bookmark_folder" => new BookmarkFolder(jsonObject)
        case "bookmark_separator" => new BookmarkSeparator(jsonObject)
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

    def createSpeedDialSlot(position: Int,
                            properties: Map[String, String]): SpeedDialSlot = {
      val json = serverProxy.post("/rest/speeddial/" + position, properties)
      return new SpeedDialSlot(JSON.parseRaw(json).get.asInstanceOf[JSONArray].list(0).asInstanceOf[JSONObject])
    }

    def updateSpeedDialSlot(position: Int,
                            properties: Map[String, String]): SpeedDialSlot = {
      val json =
        serverProxy.post("/rest/speeddial/" + position,
                         properties ++ Map("api_method" -> "update"))
      return new SpeedDialSlot(JSON.parseRaw(json).get.asInstanceOf[JSONArray].list(0).asInstanceOf[JSONObject])
    }

    def deleteSpeedDialSlot(position: Int) {
      val response =
        serverProxy.post("/rest/speeddial/" + position,
                         Map("api_method" -> "delete"))
      if (response != "") {
        throw new Exception("Error deleting Speed Dial slot " + position)
      }
    }

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

    def getBookmarks(fromFolder: Option[String] = None): Seq[BookmarkEntry] = {
      return genericGetRequest("bookmark", "children", fromFolder).asInstanceOf[Seq[BookmarkEntry]]
    }

    def getBookmarksRecursively(fromFolder: Option[String] = None): Seq[BookmarkEntry] = {
      return genericGetRequest("bookmark", "descendants", fromFolder).asInstanceOf[Seq[BookmarkEntry]]
    }

    def getBookmark(id: String): BookmarkEntry = {
      return genericGetRequest("bookmark", "", Some(id)).asInstanceOf[Seq[BookmarkEntry]](0)
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
  }
}
