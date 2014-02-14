package com.github.dunnololda.ogamer.htmlparsers

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger
import com.github.dunnololda.conn.Conn
import org.json.JSONObject
import scala.collection.mutable
import com.github.dunnololda.ogamer._

object DefenseParser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  private var token = ""

  private var defence_obtain_started = false

  val defense_mapping = Map(
    "defense1" -> "rocket-launcher",
    "defense2" -> "light-laser",
    "defense3" -> "heavy-laser",
    "defense4" -> "gaussgun",
    "defense5" -> "iongun",
    "defense6" -> "plasmagun",

    "defense7"  -> "light-dome",
    "defense8"  -> "large-dome",
    "defense9"  -> "anti-missile",
    "defense10" -> "interplanet-missile"
  )

  case class DefenseCost(metal:Int, crystal:Int, deuterium:Int)
  case class DefenseData(defense_name:String, defense_type:Int, defense_cost:DefenseCost, defense_availability:String)
  private val defenses:mutable.HashMap[String, DefenseData] = mutable.HashMap[String, DefenseData](
    "rocket-launcher" -> DefenseData("rocket-launcher",         401, DefenseCost(2000,  0,     0),     "unknown"),
    "light-laser"     -> DefenseData("light-laser",             402, DefenseCost(1500,  500,   0),     "unknown"),
    "heavy-laser"     -> DefenseData("heavy-laser",             403, DefenseCost(6000,  2000,  0),     "unknown"),
    "gaussgun"        -> DefenseData("gaussgun",                404, DefenseCost(20000, 15000, 2000),  "unknown"),
    "iongun"          -> DefenseData("iongun",                  405, DefenseCost(2000,  6000,  0),     "unknown"),
    "plasmagun"       -> DefenseData("plasmagun",               406, DefenseCost(50000, 50000, 30000), "unknown"),

    "light-dome"          -> DefenseData("light-dome",          407, DefenseCost(10000, 10000, 0),     "unknown"),
    "large-dome"          -> DefenseData("large-dome",          408, DefenseCost(50000, 50000, 0),     "unknown"),
    "anti-missile"        -> DefenseData("anti-missile",        502, DefenseCost(8000,  0,     2000),  "unknown"),
    "interplanet-missile" -> DefenseData("interplanet-missile", 503, DefenseCost(12500, 2500,  10000), "unknown")
  )

  var login_indicator = false

  private def defensesInit() {
    defenses.foreach {
      case (defense, defense_data) => defenses(defense) = defense_data.copy(defense_availability = "unknown")
    }
  }

  override def startDocument() {
    token = ""
    defence_obtain_started = false
    login_indicator = false
  }

  override def startElement(uri:String, local_name:String, raw_name:String, amap:Attributes) {
    if("input".equalsIgnoreCase(raw_name) && "hidden" == amap.getValue("type") && "token" == amap.getValue("name")) {
      token = amap.getValue("value")
    }
    if("ul".equalsIgnoreCase(raw_name) && "defensebuilding" == amap.getValue("id")) {
      defence_obtain_started = true
    }
    if("li".equalsIgnoreCase(raw_name) && defence_obtain_started) {
      if(amap.getValue("id") != null && amap.getValue("class") != null) {
        defense_mapping.get(amap.getValue("id")).foreach(defense_name => {
          defenses(defense_name) = defenses(defense_name).copy(defense_availability = amap.getValue("class"))
        })
      }
    }

    if("li".equalsIgnoreCase(raw_name) && "playerName" == amap.getValue("id")) {
      login_indicator = true
    }
  }

  override def characters(ch:Array[Char], start:Int, length:Int) {

  }

  override def endElement(uri:String, local_name:String, raw_name:String) {
     if("ul".equalsIgnoreCase(raw_name) && defence_obtain_started) {
       defence_obtain_started = false
     }
  }

  override def endDocument() {

  }

  private val parser = new Parser
  parser.setContentHandler(this)
  def parse(html:String) {
    parser.parse(new InputSource(new StringReader(html)))
  }

  def buildDefense(defense:String, amount:Int)(implicit conn:Conn, uni:String):Boolean = {
    log.info(s"trying to build $defense, $amount unit(s)")
    if(amount <= 0) {
      log.warn("amount must be above zero")
      false
    } else {
      conn.executeGet(s"http://$uni/game/index.php?page=defense")
      parse(conn.currentHtml)
      Thread.sleep((math.random*10).toInt)

      if(token == "") {
        log.warn("failed to obtain token")
        false
      } else {
        if(OverviewParser.isEmpty) {
          log.warn(s"unable to build $defense: unknown resources amount")
          false
        } else {
          defenses.get(defense) match {
            case Some(DefenseData(defense_name, defense_type, DefenseCost(metal, crystal, deuterium), defense_availability)) =>
              if(defense_availability != "on") {
                log.warn(s"unable to build $defense. Defense availability: $defense_availability")
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
                  log.warn(s"unable to build any of $defense. Unsufficient amount of resources: ${OverviewParser.metal.info}/$metal : ${OverviewParser.crystal.info}/$crystal : ${OverviewParser.deuterium.info}/$deuterium")
                  false
                } else {
                  if(available_amount < amount) {
                    log.warn(s"available amount $available_amount is less then required $amount")
                  }
                  val amount_to_build = math.min(available_amount, amount)
                  log.info(s"going to build $amount_to_build unit(s)")
                  conn.addHeader("X-Requested-With", "XMLHttpRequest")
                  val form_defense_type = new JSONObject()
                  form_defense_type.put("type", defense_type)

                  conn.addPostData(form_defense_type)
                  conn.executePost(s"http://$uni/game/index.php?page=defense&ajax=1")
                  conn.removeCustomHeader("X-Requested-With")
                  Thread.sleep((math.random*10).toInt)

                  val form_ship_data = new JSONObject()
                  form_ship_data.put("modus", 1)
                  form_ship_data.put("type", defense_type)
                  form_ship_data.put("menge", amount_to_build)
                  form_ship_data.put("token", token)
                  conn.addPostData(form_ship_data)
                  conn.executePost(s"http://$uni/game/index.php?page=defense&deprecated=1")
                  conn.executeGet(s"http://$uni/game/index.php?page=defense")
                  true
                }
              }
            case None =>
              log.warn(s"unknown defense: $defense")
              false
          }
        }
      }
    }
  }
}
