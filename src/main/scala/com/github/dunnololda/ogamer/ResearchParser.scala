package com.github.dunnololda.ogamer

import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{InputSource, Attributes}
import org.ccil.cowan.tagsoup.Parser
import java.io.StringReader
import com.github.dunnololda.cli.MySimpleLogger
import com.github.dunnololda.conn.Conn

object ResearchParser extends DefaultHandler {
  private val log = MySimpleLogger(this.getClass.getName)

  val energy_tech_research_link = new BuildLinkObtainer
  val laser_tech_research_link = new BuildLinkObtainer
  val ion_tech_research_link = new BuildLinkObtainer
  val hyper_tech_research_link = new BuildLinkObtainer
  val plasma_tech_research_link = new BuildLinkObtainer

  val reactive_engine_research_link = new BuildLinkObtainer
  val impulse_engine_research_link = new BuildLinkObtainer
  val hyper_engine_research_link = new BuildLinkObtainer

  val espionage_research_link = new BuildLinkObtainer
  val comp_tech_research_link = new BuildLinkObtainer
  val astrophysics_research_link = new BuildLinkObtainer
  val intergalactic_net_research_link = new BuildLinkObtainer
  val gravitation_tech_research_link = new BuildLinkObtainer

  val weapon_tech_research_link = new BuildLinkObtainer
  val shield_tech_research_link = new BuildLinkObtainer
  val armor_tech_research_link = new BuildLinkObtainer

  private val all_obtainers = List(
    energy_tech_research_link,
    laser_tech_research_link,
    ion_tech_research_link,
    hyper_tech_research_link,
    plasma_tech_research_link,

    reactive_engine_research_link,
    impulse_engine_research_link,
    hyper_engine_research_link,

    espionage_research_link,
    comp_tech_research_link,
    astrophysics_research_link,
    intergalactic_net_research_link,
    gravitation_tech_research_link,

    weapon_tech_research_link,
    shield_tech_research_link,
    armor_tech_research_link)

  private val mapping = Map(
    "research113" -> energy_tech_research_link,
    "research120" -> laser_tech_research_link,
    "research121" -> ion_tech_research_link,
    "research114" -> hyper_tech_research_link,
    "research122" -> plasma_tech_research_link,

    "research115" -> reactive_engine_research_link,
    "research117" -> impulse_engine_research_link,
    "research118" -> hyper_engine_research_link,

    "research106" -> espionage_research_link,
    "research108" -> comp_tech_research_link,
    "research124" -> astrophysics_research_link,
    "research123" -> intergalactic_net_research_link,
    "research199" -> gravitation_tech_research_link,

    "research109" -> weapon_tech_research_link,
    "research110" -> shield_tech_research_link,
    "research111" -> armor_tech_research_link
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


  val techs = Map(
    "energy" -> (energy_tech_research_link, "energy tech"),
    "laser" -> (laser_tech_research_link, "laser tech"),
    "ion" -> (ion_tech_research_link, "ion tech"),
    "hyper-tech" -> (hyper_tech_research_link, "hyper tech"),
    "plasma" -> (plasma_tech_research_link, "plasma tech"),

    "reactive-drive" -> (reactive_engine_research_link, "reactive drive"),
    "impulse-drive" -> (impulse_engine_research_link, "impulse drive"),
    "hyper-drive" -> (hyper_engine_research_link, "hyper drive"),

    "espionage" -> (espionage_research_link, "espionage"),
    "comp" -> (comp_tech_research_link, "comp tech"),
    "astrophysics" -> (astrophysics_research_link, "astrophysics tech"),
    "intergalactic-net" -> (intergalactic_net_research_link, "intergalactic net"),
    "gravitation" -> (gravitation_tech_research_link, "gravitation tech"),

    "weapon" -> (weapon_tech_research_link, "weapon tech"),
    "shield" -> (shield_tech_research_link, "shield tech"),
    "armor" -> (armor_tech_research_link, "armor tech")
  )

  def research(tech:String)(implicit conn:Conn, uni:String):Boolean = {
    log.info(s"trying to research $tech")
    conn.executeGet(s"http://$uni/game/index.php?page=research")
    parse(conn.currentHtml)
    techs.get(tech) match {
      case Some((link, tech_info)) =>
        if(link.isEmpty) {
          log.error(s"no $tech_info research link found!")
          false
        } else {
          conn.executeGet(link.info)
          true
        }
      case None =>
        log.error(s"unknown mine: $tech")
        false
    }
  }
}
