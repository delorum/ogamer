package com.github.dunnololda.ogamer

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger

class IntObtainer extends InfoObtainer[Int] {
  private var _info:Int = 0
  def info: Int = _info

  def obtainer2info() {
    _info = str2intOrDefault(info_obtainer.toString().trim(), 0)
  }

  def clearInfo() {
    _info = 0
  }

  def nonEmpty:Boolean = {
    _info != 0
  }
}

object OverviewParser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  val metal = new IntObtainer
  val crystal = new IntObtainer
  val deuterium = new IntObtainer
  val energy = new IntObtainer

  val all_obtainers = List(metal, crystal, deuterium, energy)

  override def startDocument() {
    all_obtainers.foreach(_.init())
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if ("div".equalsIgnoreCase(raw_name) && "message-wrapper" == amap.getValue("id")) {
      metal.info_obtain_started = true
    } else if ("span".equalsIgnoreCase(raw_name) && "resources_metal" == amap.getValue("id")) {
       metal.info_obtain_started = true
    } else if ("span".equalsIgnoreCase(raw_name) && "resources_crystal" == amap.getValue("id")) {
      crystal.info_obtain_started = true
    } else if ("span".equalsIgnoreCase(raw_name) && "resources_deuterium" == amap.getValue("id")) {
      deuterium.info_obtain_started = true
    } else if ("span".equalsIgnoreCase(raw_name) && "resources_energy" == amap.getValue("id")) {
      energy.info_obtain_started = true
    }
  }

  override def characters(ch:Array[Char], start:Int, length:Int) {
    val value = new String(ch, start, length)
    if(metal.info_obtain_started) {
      metal.append(value)
    } else if(crystal.info_obtain_started) {
      crystal.append(value)
    } else if(deuterium.info_obtain_started) {
      deuterium.append(value)
    } else if(energy.info_obtain_started) {
      energy.append(value)
    }
  }

  override def endElement(uri:String, local_name:String, raw_name:String) {
    if ("span".equalsIgnoreCase(raw_name) && metal.info_obtain_started) {
      metal.info_obtain_started = false
    } else if ("span".equalsIgnoreCase(raw_name) && crystal.info_obtain_started) {
      crystal.info_obtain_started = false
    } else if ("span".equalsIgnoreCase(raw_name) && deuterium.info_obtain_started) {
      deuterium.info_obtain_started = false
    } else if ("span".equalsIgnoreCase(raw_name) && energy.info_obtain_started) {
      energy.info_obtain_started = false
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

  def nonEmpty:Boolean = {
    all_obtainers.exists(_.nonEmpty)
  }

  def isEmpty:Boolean = !nonEmpty
}
