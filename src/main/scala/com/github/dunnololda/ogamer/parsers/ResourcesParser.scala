package com.github.dunnololda.ogamer.parsers

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger
import com.github.dunnololda.conn.Conn

abstract class InfoObtainer[T] {
  def info:T
  def obtainer2info()
  protected def clearInfo()
  def nonEmpty:Boolean
  def isEmpty = !nonEmpty

  protected val info_obtainer = new StringBuilder
  var info_obtain_started = false

  def init() {
    clearInfo()
    info_obtain_started = false
    info_obtainer.clear()
  }

  def append(str:String) {
    info_obtainer append str
  }

  override def toString:String = info.toString
}

class StringObtainer extends InfoObtainer[String] {
  protected var _info:String = ""
  def info: String = _info

  def obtainer2info() {
    _info = info_obtainer.toString()
  }

  protected def clearInfo() {
    _info = ""
  }

  def nonEmpty: Boolean = _info != ""
}

class BuildLinkObtainer extends StringObtainer {
  private val log = MySimpleLogger(this.getClass.getName)

  override def obtainer2info() {
    val test = info_obtainer.toString().trim().replace("\n", "").dropWhile(_ != '\'').dropWhile(_ == '\'').takeWhile(_ != '\'')
    try {
      new java.net.URI(test)
      _info = test
    } catch {
      case t:Throwable =>
        log.error(s"obtained build link is not a valid uri: $test")
    }
  }
}

object ResourcesParser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  val metal_build_link = new BuildLinkObtainer
  val crystal_build_link = new BuildLinkObtainer
  val deuterium_build_link = new BuildLinkObtainer
  val electro_build_link = new BuildLinkObtainer
  val thermonuclear_build_link = new BuildLinkObtainer
  val satellite_build_link = new BuildLinkObtainer

  val metal_storage_build_link = new BuildLinkObtainer
  val crystal_storage_build_link = new BuildLinkObtainer
  val deuterium_storage_build_link = new BuildLinkObtainer

  val metal_shelter_build_link = new BuildLinkObtainer
  val crystal_shelter_build_link = new BuildLinkObtainer
  val deuterium_shelter_build_link = new BuildLinkObtainer

  private val all_obtainers = List(
    metal_build_link,
    crystal_build_link,
    deuterium_build_link,
    electro_build_link,
    thermonuclear_build_link,
    satellite_build_link,

    metal_storage_build_link,
    crystal_storage_build_link,
    deuterium_storage_build_link,

    metal_shelter_build_link,
    crystal_shelter_build_link,
    deuterium_shelter_build_link)

  private val mapping = Map(
    "supply1" -> metal_build_link,
    "supply2" -> crystal_build_link,
    "supply3" -> deuterium_build_link,
    "supply4" -> electro_build_link,
    "supply12" -> thermonuclear_build_link,
    "supply212" -> satellite_build_link,

    "supply22" -> metal_storage_build_link,
    "supply23" -> crystal_storage_build_link,
    "supply24" -> deuterium_storage_build_link,

    "supply25" -> metal_shelter_build_link,
    "supply26" -> crystal_shelter_build_link,
    "supply27" -> deuterium_shelter_build_link
  )

  var login_indicator = false

  override def startDocument() {
    all_obtainers.foreach(_.init())
    login_indicator = false
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if ("div".equalsIgnoreCase(raw_name) && amap.getValue("class") != null && mapping.contains(amap.getValue("class"))) {
      mapping(amap.getValue("class")).info_obtain_started = true
    }

    if(all_obtainers.exists(_.info_obtain_started)) {
      if ("a".equalsIgnoreCase(raw_name) && amap.getValue("class") != null && amap.getValue("class").split(" ").contains("fastBuild") && amap.getValue("onclick") != null) {
        all_obtainers.find(_.info_obtain_started).foreach(_.append(amap.getValue("onclick")))
      }
    }

    if("li".equalsIgnoreCase(raw_name) && "playerName" == amap.getValue("id")) {
      login_indicator = true
    }
  }

  override def characters(ch:Array[Char], start:Int, length:Int) {

  }

  override def endElement(uri:String, local_name:String, raw_name:String) {
    if ("div".equalsIgnoreCase(raw_name)) {
      all_obtainers.filter(_.info_obtain_started).foreach(_.info_obtain_started = false)
    }
  }

  override def endDocument() {
    all_obtainers.foreach(_.obtainer2info())
  }

  private val parser = new Parser
  parser.setContentHandler(this)
  def parse(html:String) {
    parser.parse(new InputSource(new StringReader(html)))
  }

  def nonEmpty:Boolean = all_obtainers.exists(_.nonEmpty)

  def isEmpty:Boolean = !nonEmpty

  val buildings = Map(
    "metal" -> (metal_build_link, "metal mine"),
    "crystal" -> (crystal_build_link, "crystal mine"),
    "deuterium" -> (deuterium_build_link, "crystal mine"),
    "electro" -> (electro_build_link, "powerplant"),
    "thermonuclear" -> (thermonuclear_build_link, "thermonuclear powerplant"),
    "satellite" -> (satellite_build_link, "sun satellite"),

    "metal-storage" -> (metal_storage_build_link, "metal storage"),
    "crystal-storage" -> (crystal_storage_build_link, "metal storage"),
    "deuterium-storage" -> (deuterium_storage_build_link, "metal storage"),

    "metal-shelter" -> (metal_shelter_build_link, "metal shelter"),
    "crystal-shelter" -> (crystal_shelter_build_link, "metal shelter"),
    "deuterium-shelter" -> (deuterium_shelter_build_link, "metal shelter")
  )

  def buildMine(mine:String)(implicit conn:Conn, uni:String):Boolean = {
    log.info(s"trying to build mine $mine")
    conn.executeGet(s"http://$uni/game/index.php?page=resources")
    parse(conn.currentHtml)
    buildings.get(mine) match {
      case Some((link, building_info)) =>
        if(link.isEmpty) {
          log.error(s"no $building_info build link found!")
          false
        } else {
          conn.executeGet(link.info)
          true
        }
      case None =>
        log.error(s"unknown mine: $mine")
        false
    }
  }
}
