package com.github.dunnololda.ogamer

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger
import com.github.dunnololda.conn.Conn
import org.json.JSONObject

object ShipyardParser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  private var token = ""

  override def startDocument() {
    token = ""
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if("input".equalsIgnoreCase(raw_name) && "hidden" == amap.getValue("type") && "token" == amap.getValue("name")) {
      token = amap.getValue("value")
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

  def buildShip(ship:String, amount:Int)(implicit conn:Conn, uni:String):Boolean = {
    log.info(s"trying to build $ship, $amount units")
    if(amount <= 0) {
      log.warn("amount must be above zero")
      false
    } else {
      conn.executeGet(s"http://$uni/game/index.php?page=shipyard")
      parse(conn.currentHtml)
      if(token == "") {
        log.warn("failed to obtain token")
        false
      } else {
        ship match {
          case "light-interceptor" =>
            if(OverviewParser.isEmpty) {
              log.warn("unable to build ship: unknown resources amount")
              false
            } else {
              if(OverviewParser.metal.info >= 3000*amount && OverviewParser.crystal.info >= 1000*amount) {
                conn.addHeader("X-Requested-With", "XMLHttpRequest")
                val ship_type = new JSONObject()
                ship_type.put("type", 204)
                conn.addPostData(ship_type)
                conn.executePost(s"http://$uni/game/index.php?page=shipyard&ajax=1")
                conn.removeCustomHeader("X-Requested-With")
                val ship_data = new JSONObject()
                ship_data.put("modus", 1)
                ship_data.put("type", 204)
                ship_data.put("menge", amount)
                ship_data.put("token", "")
                conn.addPostData(ship_data)
                conn.executePost(s"http://$uni/game/index.php?page=shipyard&deprecated=1")
                conn.executeGet(s"http://$uni/game/index.php?page=shipyard")
                true
              } else {
                log.warn(s"unsufficient amount of resources: ${OverviewParser.metal.info}/${3000*amount} : ${OverviewParser.crystal.info}/${1000*amount} : ${OverviewParser.deuterium.info}/0")
                false
              }
            }
          case x =>
            log.warn(s"unknown ship: $x")
            false
        }
      }
    }
  }
}
