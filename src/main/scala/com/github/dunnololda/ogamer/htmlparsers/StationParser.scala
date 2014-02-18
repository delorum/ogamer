package com.github.dunnololda.ogamer.htmlparsers

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger
import com.github.dunnololda.conn.Conn

object StationParser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  val robots_build_link = new BuildLinkObtainer
  val shipyard_build_link = new BuildLinkObtainer
  val lab_build_link = new BuildLinkObtainer
  val alli_storage_build_link = new BuildLinkObtainer
  val silo_build_link = new BuildLinkObtainer
  val nanits_build_link = new BuildLinkObtainer
  val terraformer_build_link = new BuildLinkObtainer

  private val all_obtainers = List(
    robots_build_link,
    shipyard_build_link,
    lab_build_link,
    alli_storage_build_link,
    silo_build_link,
    nanits_build_link,
    terraformer_build_link)

  private val mapping = Map(
    "station14" -> robots_build_link,
    "station21" -> shipyard_build_link,
    "station31" -> lab_build_link,
    "station34" -> alli_storage_build_link,
    "station44" -> silo_build_link,
    "station15" -> nanits_build_link,
    "station33" -> terraformer_build_link
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


  val stations = Map(
    "robots" -> (robots_build_link, "robots"),
    "shipyard" -> (shipyard_build_link, "shipyard"),
    "lab" -> (lab_build_link, "research lab"),
    "alli-storage" -> (alli_storage_build_link, "alliance storage"),
    "silo" -> (silo_build_link, "thermonuclear powerplant"),
    "nanits" -> (nanits_build_link, "nanits"),
    "terraformer" -> (terraformer_build_link, "terraformer")
  )

  def buildStation(station:String)(implicit conn:Conn, uni:String):Boolean = {
    log.info(s"trying to build station $station")

    conn.executeGet(s"http://$uni/game/index.php?page=station")
    parse(conn.currentHtml)
    Thread.sleep((math.random*10).toInt*1000)

    stations.get(station) match {
      case Some((link, station_info)) =>
        if(link.isEmpty) {
          log.error(s"no $station_info build link found!")
          false
        } else {
          conn.executeGet(link.info)
          true
        }
      case None =>
        log.error(s"unknown mine: $station")
        false
    }
  }
}
