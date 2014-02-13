package com.github.dunnololda.ogamer.parsers

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger
import com.github.dunnololda.conn.Conn
import org.json.JSONObject
import scala.collection.mutable
import com.github.dunnololda.ogamer._

object Fleet3Parser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  var token = ""
  var login_indicator = false

  override def startDocument() {
    token = ""
    login_indicator = false
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if("input".equalsIgnoreCase(raw_name) && "hidden" == amap.getValue("type") && "token" == amap.getValue("name")) {
      token = amap.getValue("value")
    }

    if("li".equalsIgnoreCase(raw_name) && "playerName" == amap.getValue("id")) {
      login_indicator = true
    }
  }

   override def characters(ch:Array[Char], start:Int, length:Int) {

  }

  override def endElement(uri:String, local_name:String, raw_name:String) {

  }

  override def endDocument() {

  }

  private val parser = new Parser
  parser.setContentHandler(this)
  def parse(html:String) {
    parser.parse(new InputSource(new StringReader(html)))
  }
}
