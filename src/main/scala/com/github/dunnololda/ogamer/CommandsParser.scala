package com.github.dunnololda.ogamer

import scala.util.parsing.combinator._
import com.github.dunnololda.cli.MySimpleLogger

sealed trait Command
case class Quit(mail:Boolean) extends Command
case class SendFleet(
  mission:String,
  fleet:List[(String, Int)],
  resources:(Int,Int,Int),
  target_planet:(Int,Int,Int),
  mail:Boolean
) extends Command
case class BuildShip(ship:String, amount:Int, mail:Boolean) extends Command
case class BuildDefense(defense:String, amount:Int, mail:Boolean) extends Command
case class Limits(metal:Int, crystal:Int, deuterium:Int, mail:Boolean) extends Command
case class MaxTimeoutMin(min:Int) extends Command
case class Goto(command:Int) extends Command
case class BuildMine(mine:String, mail:Boolean) extends Command
case class BuildStation(station:String, mail:Boolean) extends Command
case class Research(tech:String, mail:Boolean) extends Command
case class MailNewMessages(send_mail:Boolean) extends Command
case object WaitFleetReturn extends Command
case class SelfFlightsLE(amount:Int) extends Command
case class FleetLimits(fleet:List[(String, Int)], else_goto:Option[Int], mail:Boolean) extends Command
case object Return extends Command
case object EmptyString extends Command
case object Comment extends Command
case class UnknownCommand(command:String) extends Command
case object LogHtml extends Command
case class SetPlanet(planet:String) extends Command

object CommandsParser extends JavaTokenParsers {
  private val log = MySimpleLogger(this.getClass.getName)

  def maybeMailParser:Parser[Boolean] = opt("mail") ^^ {case x => x.exists(_ == "mail")}
  def maybeNoMailParser:Parser[Boolean] = opt("nomail") ^^ {case x => x.exists(_ == "nomail")}
  def missionParser:Parser[String] = "spy" | "attack" | "transport" | "stay-on"
  def shipParser:Parser[String] =
    "light-interceptor" |
    "heavy-interceptor" |
    "cruiser" |
    "battleship" |
    "battlecruiser" |
    "bomber" |
    "exterminator" |
    "deathstar" |
    "small-transport" |
    "large-transport" |
    "colonizer" |
    "processor" |
    "probe" |
    "satellite"
  def shipTypeAndAmountParser:Parser[(String, Int)] = shipParser~":"~wholeNumber ^^ {case ship~":"~amount => (ship, amount.toInt)}
  def fleetParser:Parser[List[(String, Int)]] = repsep(shipTypeAndAmountParser, ",") ^^ {case x => x}
  def resourcesParser:Parser[(Int,Int,Int)] = wholeNumber~":"~wholeNumber~":"~wholeNumber ^^ {case m~":"~c~":"~d => (m.toInt, c.toInt, d.toInt)}
  def planetParser:Parser[(Int,Int,Int)] = wholeNumber~":"~wholeNumber~":"~wholeNumber ^^ {case g~":"~s~":"~p => (g.toInt, s.toInt, p.toInt)}
  def defenseParser:Parser[String] =
    "rocket-launcher" |
    "light-laser" |
    "heavy-laser" |
    "gaussgun" |
    "iongun" |
    "plasmagun" |
    "light-dome" |
    "heavy-dome" |
    "anti-missile" |
    "interplanet-missile"
  def mineParser:Parser[String] =
    "metal" |
    "crystal" |
    "deuterium" |
    "electro" |
    "thermonuclear" |
    "metal-storage" |
    "crystal-storage" |
    "deuterium-storage" |
    "metal-shelter" |
    "crystal-shelter" |
    "deuterium-shelter"
  def stationParser:Parser[String] =
    "robots" |
    "shipyard" |
    "lab" |
    "alli-storage" |
    "silo" |
    "nanits" |
    "terraformer"
  def techParser:Parser[String] =
    "energy" |
    "laser" |
    "ion" |
    "hyper-tech" |
    "plasma" |
    "reactive-drive" |
    "impulse-drive" |
    "hyper-drive" |
    "espionage" |
    "comp" |
    "astrophysics" |
    "intergalactic-net" |
    "gravitation" |
    "weapon" |
    "shield" |
    "armor"

  def quitParser:Parser[Quit] = "quit"~maybeNoMailParser ^^ {
    case "quit"~no_mail=>
      Quit(!no_mail)
  }

  def sendFleetParser:Parser[SendFleet] = "send"~missionParser~fleetParser~resourcesParser~planetParser~maybeMailParser ^^ {
    case "send"~mission~fleet~resources~target_planet~mail => SendFleet(mission, fleet, resources, target_planet, mail)
  }

  def buildShipParser:Parser[BuildShip] = "build-ship"~shipParser~wholeNumber~maybeNoMailParser ^^ {
    case "build-ship"~ship~amount~no_mail => BuildShip(ship, amount.toInt, !no_mail)
  }

  def buildDefenseParser:Parser[BuildDefense] = "build-defense"~defenseParser~wholeNumber~maybeNoMailParser ^^ {
    case "build-defense"~defense~amount~no_mail => BuildDefense(defense, amount.toInt, !no_mail)
  }
  
  def limitsParser:Parser[Limits] = "limits"~resourcesParser~maybeMailParser ^^ {
    case "limits"~resources~mail => Limits(resources._1, resources._2, resources._3, mail)
  }

  def maxTimeoutMinParser:Parser[MaxTimeoutMin] = "max-timeout-min"~wholeNumber ^^ {case "max-timeout-min"~min => MaxTimeoutMin(min.toInt)}

  def gotoParser:Parser[Goto] = "goto"~wholeNumber ^^ {case "goto"~goto => Goto(goto.toInt)}

  def buildMineParser:Parser[BuildMine] = "build-mine"~mineParser~maybeNoMailParser ^^ {case "build-mine"~mine~no_mail => BuildMine(mine, !no_mail)}

  def buildStationParser:Parser[BuildStation] = "build-station"~stationParser~maybeNoMailParser ^^ {
    case "build-station"~station~no_mail => BuildStation(station, !no_mail)
  }

  def researchParser:Parser[Research] = "research"~techParser~maybeNoMailParser ^^ {case "research"~tech~no_mail => Research(tech, !no_mail)}

  def fleetLimistParser:Parser[FleetLimits] = "fleet-limits"~fleetParser~maybeMailParser~opt("else-goto"~wholeNumber) ^^ {
    case "fleet-limits"~fleet~mail~maybe_else_goto => FleetLimits(fleet, maybe_else_goto.map {
      case "else-goto"~else_goto => else_goto.toInt
    }, mail)
  }

  def waitFleetReturnParser:Parser[WaitFleetReturn.type] = "wait-fleet-return" ^^ {case _ => WaitFleetReturn}

  def selfFlightsLEParser:Parser[SelfFlightsLE] = "self-flights-le"~wholeNumber ^^ {case "self-flights-le"~amount => SelfFlightsLE(amount.toInt)}

  def mailNewMessagesParser:Parser[MailNewMessages] = "mail-new-messages"~("on" | "off") ^^ {case "mail-new-messages"~x => MailNewMessages("on" == x)}

  def returnParser:Parser[Return.type] = "return" ^^ {case _ => Return}

  def loghtmlParser:Parser[LogHtml.type] = "log-html" ^^ {case _ => LogHtml}

  def setPlanetParser:Parser[SetPlanet] = "set-planet"~planetParser ^^ {
    case "set-planet"~planet =>
      val (g, s, p) = planet
      SetPlanet(s"$g:$s:$p")
  }

  def commentParser:Parser[Comment.type] = "#"~".*".r ^^ {case "#"~s => Comment}

  def emptyStringParser:Parser[EmptyString.type] = " *".r ^^ {case s => EmptyString}

  def commandParser:Parser[Command] =
    quitParser            |
    sendFleetParser       |
    buildShipParser       |
    buildDefenseParser    |
    limitsParser          |
    maxTimeoutMinParser   |
    gotoParser            |
    buildMineParser       |
    buildStationParser    |
    researchParser        |
    fleetLimistParser     |
    waitFleetReturnParser |
    selfFlightsLEParser   |
    mailNewMessagesParser |
    returnParser          |
    loghtmlParser         |
    setPlanetParser       |
    commentParser         |
    emptyStringParser

  def loadCommands(commands_filename:String):Array[Command] = {
    val commands_file = new java.io.File(commands_filename)
    if(!commands_file.exists()) Array()
    else {
      try {
        (for {
          line <- io.Source.fromFile(commands_filename).getLines()
        } yield {
          parseAll(commandParser, line) match {
            case Success(command, _) => command
            case Failure(msg, _) =>
              log.error(s"failed to parse $line: $msg")
              UnknownCommand(line)
            case Error(msg, _) =>
              log.error(s"failed to parse $line: $msg")
              UnknownCommand(line)
          }
        }).toArray
      } catch {
        case t:Throwable =>
          log.error(s"failed to load existing commands file: $t")
          Array()
      }
    }
  }
}
