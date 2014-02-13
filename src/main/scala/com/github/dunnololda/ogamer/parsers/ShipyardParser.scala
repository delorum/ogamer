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
import scala.Some

object ShipyardParser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  private var token = ""

  private var military_ships_obtain_started = false
  private var civilian_ships_obtain_started = false

  val ships_mapping = Map(
    "military-button1" -> "light-interceptor",
    "military-button2" -> "heavy-interceptor",
    "military-button3" -> "cruiser",
    "military-button4" -> "battleship",

    "military-button5" -> "battlecruiser",
    "military-button6" -> "bomber",
    "military-button7" -> "exterminator",
    "military-button8" -> "deathstar",

    "civil-button1" -> "small-transport",
    "civil-button2" -> "large-transport",
    "civil-button3" -> "colonizer",

    "civil-button4" -> "processor",
    "civil-button5" -> "probe",
    "civil-button6" -> "sun-satellite"
  )

  case class ShipCost(metal:Int, crystal:Int, deuterium:Int)
  case class ShipData(ship_name:String, ship_type:Int, ship_cost:ShipCost, ship_availability:String)
  private val ships:mutable.HashMap[String, ShipData] = mutable.HashMap[String, ShipData](
    "light-interceptor" -> ShipData("light-interceptor", 204, ShipCost(3000,  1000, 0),     "unknown"),
    "heavy-interceptor" -> ShipData("heavy-interceptor", 205, ShipCost(6000,  4000, 0),     "unknown"),
    "cruiser"           -> ShipData("cruiser",           206, ShipCost(20000, 7000, 2000),  "unknown"),
    "battleship"        -> ShipData("battleship",        207, ShipCost(45000, 15000, 0),    "unknown"),

    "battlecruiser"     -> ShipData("battlecruiser",     215, ShipCost(30000,   40000,   15000),   "unknown"),
    "bomber"            -> ShipData("bomber",            211, ShipCost(50000,   25000,   15000),   "unknown"),
    "exterminator"      -> ShipData("exterminator",      213, ShipCost(60000,   50000,   15000),   "unknown"),
    "deathstar"         -> ShipData("deathstar",         214, ShipCost(5000000, 4000000, 1000000), "unknown"),

    "small-transport"   -> ShipData("small-transport",   202, ShipCost(2000,  2000,  0),     "unknown"),
    "large-transport"   -> ShipData("large-transport",   203, ShipCost(6000,  6000,  0),     "unknown"),
    "colonizer"         -> ShipData("colonizer",         208, ShipCost(10000, 20000, 10000), "unknown"),

    "processor"         -> ShipData("processor",         209, ShipCost(10000, 6000, 2000), "unknown"),
    "probe"             -> ShipData("probe",             210, ShipCost(0,     1000, 0),    "unknown"),
    "sun-satellite"     -> ShipData("sun-satellite",     212, ShipCost(0,     2000, 500),  "unknown")
  )

  private def shipsInit() {
    ships.foreach {
      case (ship, ship_data) => ships(ship) = ship_data.copy(ship_availability = "unknown")
    }
  }

  var login_indicator = false

  override def startDocument() {
    token = ""
    military_ships_obtain_started = false
    civilian_ships_obtain_started = false
    login_indicator = false
    shipsInit()
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if("input".equalsIgnoreCase(raw_name) && "hidden" == amap.getValue("type") && "token" == amap.getValue("name")) {
      token = amap.getValue("value")
    }
    if("ul".equalsIgnoreCase(raw_name)) {
      if("military" == amap.getValue("id")) {
        military_ships_obtain_started = true
      }

      if("civil" == amap.getValue("id")) {
        civilian_ships_obtain_started = true
      }
    }
    if("li".equalsIgnoreCase(raw_name) && (military_ships_obtain_started || civilian_ships_obtain_started)) {
      if(military_ships_obtain_started) {
        if(amap.getValue("id") != null && amap.getValue("class") != null) {
          ships_mapping.get(s"military-${amap.getValue("id")}").foreach(ship_name => {
            ships(ship_name) = ships(ship_name).copy(ship_availability = amap.getValue("class"))
          })
        }
      } else if(civilian_ships_obtain_started) {
        if(amap.getValue("id") != null && amap.getValue("class") != null) {
          ships_mapping.get(s"civil-${amap.getValue("id")}").foreach(ship_name => {
            ships(ship_name) = ships(ship_name).copy(ship_availability = amap.getValue("class"))
          })
        }
      }
    }

    if("li".equalsIgnoreCase(raw_name) && "playerName" == amap.getValue("id")) {
      login_indicator = true
    }
  }

  override def characters(ch:Array[Char], start:Int, length:Int) {

  }

  override def endElement(uri:String, local_name:String, raw_name:String) {
    if("ul".equalsIgnoreCase(raw_name) && (military_ships_obtain_started || civilian_ships_obtain_started)) {
      if(military_ships_obtain_started) military_ships_obtain_started = false
      if(civilian_ships_obtain_started) civilian_ships_obtain_started = false
    }
  }

  override def endDocument() {

  }

  private val parser = new Parser
  parser.setContentHandler(this)
  def parse(html:String) {
    parser.parse(new InputSource(new StringReader(html)))
  }

  def buildShip(ship:String, amount:Int)(implicit conn:Conn, uni:String):Boolean = {
    log.info(s"trying to build $ship, $amount unit(s)")
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
        if(OverviewParser.isEmpty) {
          log.warn(s"unable to build $ship: unknown resources amount")
          false
        } else {
          ships.get(ship) match {
            case Some(ShipData(ship_name, ship_type, ShipCost(metal, crystal, deuterium), ship_availability)) =>
              if(ship_availability != "on") {
                log.warn(s"unable to build $ship. Ship availability: $ship_availability")
                false
              } else {
                val available_amount = math.min(
                  math.min(
                    safeDelete(OverviewParser.metal.info,   metal),
                    safeDelete(OverviewParser.crystal.info, crystal)
                  ),
                  safeDelete(OverviewParser.deuterium.info, deuterium)
                )
                if(available_amount == 0) {
                  log.warn(s"unable to build any of $ship. Unsufficient amount of resources: ${OverviewParser.metal.info}/$metal : ${OverviewParser.crystal.info}/$crystal : ${OverviewParser.deuterium.info}/$deuterium")
                  false
                } else {
                  if(available_amount < amount) {
                    log.warn(s"available amount $available_amount is less then required $amount")
                  }
                  val amount_to_build = math.min(available_amount, amount)
                  log.info(s"going to build $amount_to_build unit(s)")
                  conn.addHeader("X-Requested-With", "XMLHttpRequest")
                  val form_ship_type = new JSONObject()
                  form_ship_type.put("type", ship_type)
                  conn.addPostData(form_ship_type)
                  conn.executePost(s"http://$uni/game/index.php?page=shipyard&ajax=1")
                  conn.removeCustomHeader("X-Requested-With")
                  val form_ship_data = new JSONObject()
                  form_ship_data.put("modus", 1)
                  form_ship_data.put("type", ship_type)
                  form_ship_data.put("menge", amount_to_build)
                  form_ship_data.put("token", token)
                  conn.addPostData(form_ship_data)
                  conn.executePost(s"http://$uni/game/index.php?page=shipyard&deprecated=1")
                  conn.executeGet(s"http://$uni/game/index.php?page=shipyard")
                  true
                }
              }
            case None =>
              log.warn(s"unknown ship: $ship")
              false
          }
        }
      }
    }
  }
}
