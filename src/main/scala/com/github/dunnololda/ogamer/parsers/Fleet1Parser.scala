package com.github.dunnololda.ogamer.parsers

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger
import com.github.dunnololda.conn.Conn
import scala.collection.mutable
import org.json.JSONObject

object Fleet1Parser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)
  var login_indicator = false

  private val fleet_mapping = Map(
    "button204" -> "light-interceptor",
    "button205" -> "heavy-interceptor",
    "button206" -> "cruiser",
    "button207" -> "battleship",

    "button215" -> "battlecruiser",
    "button211" -> "bomber",
    "button213" -> "exterminator",
    "button214" -> "deathstar",

    "button202" -> "small-transport",
    "button203" -> "large-transport",
    "button208" -> "colonizer",

    "button209" -> "processor",
    "button210" -> "probe"
  )

  private var current_parsing_ship = ""
  private val ship_amount_obtainer = new IntObtainer

  private var textlabel_started = false
  private var textlabel_skipped = false

  case class ShipInfo(ship_name:String, ship_type:Int, ship_capacity:Int, amount:Int, available:Boolean)
  private val fleet_info = mutable.HashMap[String, ShipInfo](
    "light-interceptor" -> ShipInfo("light-interceptor", 204, 50,   0, available = false),
    "heavy-interceptor" -> ShipInfo("heavy-interceptor", 205, 100,  0, available = false),
    "cruiser"           -> ShipInfo("cruiser",           206, 800,  0, available = false),
    "battleship"        -> ShipInfo("battleship",        207, 1500, 0, available = false),

    "battlecruiser"     -> ShipInfo("battlecruiser",     215, 750,     0, available = false),
    "bomber"            -> ShipInfo("bomber",            211, 500,     0, available = false),
    "exterminator"      -> ShipInfo("exterminator",      213, 2000,    0, available = false),
    "deathstar"         -> ShipInfo("deathstar",         214, 1000000, 0, available = false),

    "small-transport"   -> ShipInfo("small-transport",   202, 5000,  0, available = false),
    "large-transport"   -> ShipInfo("large-transport",   203, 25000, 0, available = false),
    "colonizer"         -> ShipInfo("colonizer",         208, 7500,  0, available = false),

    "processor"         -> ShipInfo("processor",         209, 20000, 0, available = false),
    "probe"             -> ShipInfo("probe",             210, 5,     0, available = false),
    "sun-satellite"     -> ShipInfo("sun-satellite",     212, 0,     0, available = false)
  )

  private val mission_types = Map(
    "spy"       -> 6,
    "attack"    -> 1,
    "transport" -> 3,
    "stay-on"   -> 4
  )

  private def fleetInit() {
    fleet_info.foreach {
      case (ship, ship_info) => fleet_info(ship) = ship_info.copy(amount = 0, available = false)
    }
  }

  override def startDocument() {
    login_indicator = false
    current_parsing_ship = ""
    ship_amount_obtainer.init()
    textlabel_started = false
    textlabel_skipped = false
    fleetInit()
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if("li".equalsIgnoreCase(raw_name) && "playerName" == amap.getValue("id")) {
      login_indicator = true
    }

    if("li".equalsIgnoreCase(raw_name) && amap.getValue("id") != null && fleet_mapping.contains(amap.getValue("id"))) {
      fleet_mapping.get(amap.getValue("id")).foreach(ship_name => {
        fleet_info(ship_name) = fleet_info(ship_name).copy(available = "on" == amap.getValue("class"))
      })
      current_parsing_ship = fleet_mapping.get(amap.getValue("id")).getOrElse("")
    }

    if("span".equalsIgnoreCase(raw_name) && current_parsing_ship != "" && "level" == amap.getValue("class")) {
      ship_amount_obtainer.init()
      textlabel_started = false
      textlabel_skipped = false
      ship_amount_obtainer.info_obtain_started = true
    }

    if("span".equalsIgnoreCase(raw_name) && "textlabel" == amap.getValue("class")) {
      textlabel_started = true
    }
  }

  override def characters(ch:Array[Char], start:Int, length:Int) {
    val value = new String(ch, start, length)
    if(current_parsing_ship != "" && ship_amount_obtainer.info_obtain_started && textlabel_skipped) {
      ship_amount_obtainer.append(value)
    }
  }

  override def endElement(uri:String, local_name:String, raw_name:String) {
    if("span".equalsIgnoreCase(raw_name) && textlabel_skipped && current_parsing_ship != "" && ship_amount_obtainer.info_obtain_started) {
      ship_amount_obtainer.info_obtain_started = false
      ship_amount_obtainer.obtainer2info()
      fleet_info(current_parsing_ship) = fleet_info(current_parsing_ship).copy(amount = ship_amount_obtainer.info)
    }

    if("span".equalsIgnoreCase(raw_name) && textlabel_started) {
      textlabel_skipped = true
    }

      if("li".equalsIgnoreCase(raw_name) && current_parsing_ship != "") {
      current_parsing_ship = ""
    }
  }

  override def endDocument() {

  }

  private val parser = new Parser
  parser.setContentHandler(this)
  def parse(html:String) {
    parser.parse(new InputSource(new StringReader(html)))
  }

  def sendFleet(mission:String, fleet:List[(String, Int)], resources:(Int, Int, Int), from_planet:(Int,Int,Int), target_planet:(Int,Int,Int))(implicit conn:Conn, uni:String):Boolean = {
    def fleetAddition(form_fleetx:JSONObject):List[Boolean] = {
      fleet.map {
        case (ship, amount) =>
          fleet_info.get(ship) match {
            case Some(ShipInfo(ship_name, ship_type, ship_capacity, overall_amount, available)) =>
              if(available && overall_amount >= amount) {
                form_fleetx.put(s"am$ship_type", amount)
                true
              } else {
                log.warn(s"cannot send $amount ship $ship_name: availability = $available, overall amount = $overall_amount")
                false
              }
            case None =>
              log.warn(s"unknown ship: $ship")
              false
          }
      }
    }
    
    log.info(s"going to send fleet:\n" +
      s"from planet: ${from_planet._1}:${from_planet._2}:${from_planet._3}\n" +
      s"fleet: ${fleet.map(x => s"${x._1}:${x._2}").mkString(", ")}\n" +
      s"mission: $mission\n" +
      s"resources: m:${resources._1} c:${resources._2} d:${resources._3}\n" +
      s"target planet: ${target_planet._1}:${target_planet._2}:${target_planet._3}"
    )

    conn.executeGet(s"http://$uni/game/index.php?page=fleet1")
    parse(conn.currentHtml)

    val form_fleet2 = new JSONObject()
    form_fleet2.put("galaxy",   from_planet._1)
    form_fleet2.put("system",   from_planet._2)
    form_fleet2.put("position", from_planet._3)
    form_fleet2.put("type",     1)
    form_fleet2.put("mission",  0)
    form_fleet2.put("speed",    10)
    val fleet2_addition = fleetAddition(form_fleet2)
    
    if(fleet2_addition.exists(x => x)) {
      conn.addPostData(form_fleet2)
      conn.executePost(s"http://$uni/game/index.php?page=fleet2")

      val form_fleet3 = new JSONObject()
      form_fleet3.put("type",    1)
      form_fleet3.put("mission", 0)
      form_fleet3.put("union",   0)
      val fleet3_addition = fleetAddition(form_fleet3)
      if(fleet3_addition.exists(x => x)) {
        val (galaxy, system, position) = target_planet
        form_fleet3.put("galaxy",    galaxy)
        form_fleet3.put("system",    system)
        form_fleet3.put("position",  position)
        form_fleet3.put("acsValues", "-")
        form_fleet3.put("speed",     10)

        conn.addPostData(form_fleet3)
        conn.executePost(s"http://$uni/game/index.php?page=fleet3")
        Fleet3Parser.parse(conn.currentHtml)
        if(Fleet3Parser.token == "") {
          log.warn("failed to obtain token")
          false
        } else {
          val form_movement = new JSONObject()
          form_movement.put("holdingtime", 1)
          form_movement.put("token",       Fleet3Parser.token)
          form_movement.put("galaxy",      galaxy)
          form_movement.put("system",      system)
          form_movement.put("position",    position)
          form_movement.put("type",        1)

          mission_types.get(mission) match {
            case Some(mission_type) =>
              form_movement.put("mission",          mission_type)
              form_movement.put("union2",           0)
              form_movement.put("holdingOrExpTime", 0)
              form_movement.put("speed",            10)
              form_movement.put("acsValues",        "-")
              val movement_addition = fleetAddition(form_movement)
              if(movement_addition.exists(x => x)) {
                val (metal, crystal, deuterium) = resources
                val needed_capacity = metal+crystal+deuterium

                val fleet_capacity = fleet.map {
                  case (ship, amount) =>
                    fleet_info.get(ship) match {
                      case Some(ShipInfo(ship_name, ship_type, ship_capacity, overall_amount, available)) =>
                        ship_capacity*amount
                      case None =>
                        log.warn(s"unknown ship: $ship")
                        0
                    }
                }.sum

                if(needed_capacity > fleet_capacity) {
                  log.error(s"unsufficient fleet capacity: $fleet_capacity, need: $needed_capacity")
                  false
                } else {
                  if(OverviewParser.isEmpty) {
                    log.error(s"unable to send fleet: unknown resources amount")
                    false
                  } else {
                    if(metal <= OverviewParser.metal.info && crystal <= OverviewParser.crystal.info && deuterium <= OverviewParser.deuterium.info) {
                      form_movement.put("metal",     metal)
                      form_movement.put("crystal",   crystal)
                      form_movement.put("deuterium", deuterium)

                      conn.addPostData(form_movement)
                      conn.executePost(s"http://$uni/game/index.php?page=movement")
                      conn.executeGet(s"http://$uni/game/index.php?page=fleet1")

                      true
                    } else {
                      log.error(s"unable to send fleet. Unsufficient amount of resources: ${OverviewParser.metal.info}/$metal : ${OverviewParser.crystal.info}/$crystal : ${OverviewParser.deuterium.info}/$deuterium")
                      false
                    }
                  }
                }
              } else {
                log.error("didn't add any ship, so nothing to send")
                false
              }              
            case None =>
              log.error(s"unknown mission type: $mission")
              false
          }
        }
      } else {
        log.error("didn't add any ship, so nothing to send")
        false
      }
    } else {
      log.error("didn't add any ship, so nothing to send")
      false
    }
  }
}
