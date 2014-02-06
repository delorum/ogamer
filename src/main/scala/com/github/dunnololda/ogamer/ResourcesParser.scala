package com.github.dunnololda.ogamer

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader

abstract class InfoObtainer[T] {
  def info:T
  def obtainer2info()
  def clearInfo()
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

  def clearInfo() {
    _info = ""
  }

  def nonEmpty: Boolean = _info != ""
}

class BuildLinkObtainer extends StringObtainer {
  override def obtainer2info() {
    _info = info_obtainer.toString().trim().replace("\n", "").replace("sendBuildRequest('", "").replace("', null, 1);", "")
  }
}

object ResourcesParser extends DefaultHandler {
  val metal_build_link = new BuildLinkObtainer
  val crystal_build_link = new BuildLinkObtainer
  val deuterium_build_link = new BuildLinkObtainer
  val electro_build_link = new BuildLinkObtainer

  val all_obtainers = List(metal_build_link, crystal_build_link, deuterium_build_link, electro_build_link)

  override def startDocument() {
    all_obtainers.foreach(_.init())
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if ("div".equalsIgnoreCase(raw_name) && "supply1" == amap.getValue("class")) {
      metal_build_link.info_obtain_started = true
    } else if ("div".equalsIgnoreCase(raw_name) && "supply2" == amap.getValue("class")) {
      crystal_build_link.info_obtain_started = true
    } else if ("div".equalsIgnoreCase(raw_name) && "supply3" == amap.getValue("class")) {
      deuterium_build_link.info_obtain_started = true
    } else if ("div".equalsIgnoreCase(raw_name) && "supply4" == amap.getValue("class")) {
      electro_build_link.info_obtain_started = true
    }

    if(all_obtainers.exists(_.info_obtain_started)) {
      if ("a".equalsIgnoreCase(raw_name) && amap.getValue("class") != null && amap.getValue("class").startsWith("fastBuild")) {
        if(metal_build_link.info_obtain_started) {
          metal_build_link.append(amap.getValue("onclick"))
        } else if(crystal_build_link.info_obtain_started) {
          crystal_build_link.append(amap.getValue("onclick"))
        } else if(deuterium_build_link.info_obtain_started) {
          deuterium_build_link.append(amap.getValue("onclick"))
        } else if(electro_build_link.info_obtain_started) {
          electro_build_link.append(amap.getValue("onclick"))
        }
      }
    }
  }

  override def characters(ch:Array[Char], start:Int, length:Int) {

  }

  override def endElement(uri:String, local_name:String, raw_name:String) {
    if ("div".equalsIgnoreCase(raw_name)) {
      if(metal_build_link.info_obtain_started) {
        metal_build_link.info_obtain_started = false
      } else if(crystal_build_link.info_obtain_started) {
        crystal_build_link.info_obtain_started = false
      } else if(deuterium_build_link.info_obtain_started) {
        deuterium_build_link.info_obtain_started = false
      } else if(electro_build_link.info_obtain_started) {
        electro_build_link.info_obtain_started = false
      }  
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
}
