package com.github.dunnololda.ogamer

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger

object OverviewParser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  private var metal_obtained = false
  private val metal_str = new StringBuilder
  var metal:Int = 0

  private var crystal_obtained = false
  private val crystal_str = new StringBuilder
  var crystal:Int = 0

  private var deuterium_obtained = false
  private val deuterium_str = new StringBuilder
  var deuterium:Int = 0

  override def startDocument() {
    metal = 0
    metal_str.clear()

    crystal = 0
    crystal_str.clear()

    deuterium = 0
    deuterium_str.clear()
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if ("span".equalsIgnoreCase(raw_name) && "resources_metal" == amap.getValue("id")) {
       metal_obtained = true
    } else if ("span".equalsIgnoreCase(raw_name) && "resources_crystal" == amap.getValue("id")) {
      crystal_obtained = true
    } else if ("span".equalsIgnoreCase(raw_name) && "resources_deuterium" == amap.getValue("id")) {
      deuterium_obtained = true
    }
  }

  override def characters(ch:Array[Char], start:Int, length:Int) {
    val value = new String(ch, start, length)
    if(metal_obtained) {
      metal_str.append(value)
    } else if(crystal_obtained) {
      crystal_str.append(value)
    } else if(deuterium_obtained) {
      deuterium_str.append(value)
    }
  }

  override def endElement(uri:String, local_name:String, raw_name:String) {
    if ("span".equalsIgnoreCase(raw_name) && metal_obtained) {
      metal_obtained = false
      metal = str2intOrDefault(metal_str.toString(), metal)
    } else if ("span".equalsIgnoreCase(raw_name) && crystal_obtained) {
      crystal_obtained = false
      crystal = str2intOrDefault(crystal_str.toString(), crystal)
    } else if ("span".equalsIgnoreCase(raw_name) && deuterium_obtained) {
      deuterium_obtained = false
      deuterium = str2intOrDefault(deuterium_str.toString(), deuterium)
    }
  }

  override def endDocument() {}

  private val parser = new Parser
  parser.setContentHandler(this)
  def parse(html:String) {
    parser.parse(new InputSource(new StringReader(html)))
  }

  def clear() {
    metal = 0
    crystal = 0
    deuterium = 0
  }

  def nonEmpty:Boolean = {
    metal != 0 || crystal != 0 || deuterium != 0
  }

  def isEmpty:Boolean = !nonEmpty
}
