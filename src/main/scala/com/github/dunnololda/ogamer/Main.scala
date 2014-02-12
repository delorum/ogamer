package com.github.dunnololda.ogamer

import com.github.dunnololda.conn.Conn
import org.json.JSONObject
import com.github.dunnololda.cli.Imports._
import akka.actor.{Props, Actor, ActorSystem}
import concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends Cli {
  programDescription = s"Ogame Controller v$appVersion"
  commandLineArgsAndParse(
    ("u", "uni", "ogame universe to log in", true, true),
    ("l", "login", "ogame user login", true, true),
    ("p", "pass", "ogame user password", true, true),
    ("g", "gmail-login", "gmail login to send notifications to", true, true),
    ("gp", "gmail-pass", "gmail password", true, true)
  )

  val uni = stringProperty("uni")
  val login = stringProperty("login")
  val pass = stringProperty("pass")
  val gmail_login = stringProperty("gmail-login")
  val gmail_pass = stringProperty("gmail-pass")

  private val system = ActorSystem("ogame")
  system.actorOf(Props(new Master(login, pass, gmail_login, gmail_pass)(uni)), name = "master")

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run() {
      system.shutdown()
      system.awaitTermination()
    }
  })
}

case object EnterSite
case object PerformLogin

case object Overview
case object Resources
case object Station
case object Research
case object Shipyard
case object Defense
case object Fleet

class Master(login:String, pass:String, gmail_login:String, gmail_pass:String)(implicit uni:String) extends Actor {
  private val log = MySimpleLogger(this.getClass.getName)

  private val all_pages = List(Overview, Resources, Station, Research, Shipyard, Defense, Fleet)
  def randomPage = {
    if(math.random < 0.3) Overview
    else all_pages((math.random*all_pages.length).toInt)
  }

  private var max_timeout_between_check_seconds = 5*60   // 5 minutes in seconds
  private var current_command_number = 0

  private implicit val conn = new Conn
  private var is_logged_in = false

  override def preStart() {
    log.info(s"starting actor ${self.path.toString}")
    self ! EnterSite
  }

  override def postStop() {
    log.info(s"${self.path.toString} died!")
  }

  private def scheduleNextCheck() {
    val random_wait_time = (math.random*max_timeout_between_check_seconds+1).toLong
    log.info(s"performing next check in ${duration2str(random_wait_time)}")
    context.system.scheduler.scheduleOnce(delay = random_wait_time.seconds) {
      self ! randomPage
    }
  }

  private def commandsCheck() {
    val commands = loadCommands
    if(commands.isEmpty) {
      log.info("no commands found")
      scheduleNextCheck()
    } else {
      if(current_command_number >= 0 && current_command_number < commands.length) {
        val command = commands(current_command_number)
        log.info(s"current command: ${current_command_number+1} : $command")
        val command_split = command.split(" ")
        if(command_split.length > 2) {
          command_split(0) match {
            case "build-ship" =>
              val ship = command_split(1)
              val amount = str2intOrDefault(command_split(2), 0)
              val result = ShipyardParser.buildShip(ship, amount)
              if(result) {
                if(command_split.length > 3 && "mail" == command_split(3)) {
                  sendMailSimple(gmail_login, gmail_pass,
                    "started to build ship",
                    s"started to build ship $ship")
                }
                current_command_number += 1
              }
              scheduleNextCheck()
            case "build-defense" =>
              val defense = command_split(1)
              val amount = str2intOrDefault(command_split(2), 0)
              val result = DefenseParser.buildDefense(defense, amount)
              if(result) {
                if(command_split.length > 3 && "mail" == command_split(3)) {
                  sendMailSimple(gmail_login, gmail_pass,
                    "started to build defense",
                    s"started to build defense $defense")
                }
                current_command_number += 1
              }
              scheduleNextCheck()
            case x =>
              log.warn(s"unknown command: $command")
              current_command_number += 1
              commandsCheck()
          }
        } else if(command_split.length > 1) {
          command_split(0) match {
            case "limits" =>
              val limits = command_split(1).split(":")
              if(limits.length == 3) {
                val metal_limit = str2intOrDefault(limits(0), 0)
                val crystal_limit = str2intOrDefault(limits(1), 0)
                val deuterium_limit = str2intOrDefault(limits(2), 0)
                val need_to_reach_limit = metal_limit != 0 || crystal_limit != 0 || deuterium_limit != 0
                if(need_to_reach_limit) {
                  val metal_limit_reached = metal_limit == 0 || OverviewParser.metal.info >= metal_limit
                  val crystal_limit_reached = crystal_limit == 0 || OverviewParser.crystal.info >= crystal_limit
                  val deuterium_limit_reached = deuterium_limit == 0 || OverviewParser.deuterium.info >= deuterium_limit
                  if(metal_limit_reached && crystal_limit_reached && deuterium_limit_reached) {
                    log.info(s"limits reached: ${OverviewParser.metal.info}/$metal_limit : ${OverviewParser.crystal.info}/$crystal_limit : ${OverviewParser.deuterium.info}/$deuterium_limit")
                    if(command_split.length > 2 && "mail" == command_split(2)) {
                      sendMailSimple(gmail_login, gmail_pass,
                        "limits reached",
                        s"${OverviewParser.metal.info}/$metal_limit : ${OverviewParser.crystal.info}/$crystal_limit : ${OverviewParser.deuterium.info}/$deuterium_limit")
                    }
                    current_command_number += 1
                    commandsCheck()
                  } else {
                    log.info(s"limits not reached: ${OverviewParser.metal.info}/$metal_limit : ${OverviewParser.crystal.info}/$crystal_limit : ${OverviewParser.deuterium.info}/$deuterium_limit")
                    scheduleNextCheck()
                  }
                } else {
                  log.error(s"no limits set: $command")
                  current_command_number += 1
                  commandsCheck()
                }
              } else {
                log.error(s"no limits set: $command")
                current_command_number += 1
                commandsCheck()
              }
            case "max-timeout-min" =>
              val max_timeout_min = str2intOrDefault(command_split(1), 0)
              if(max_timeout_min > 0) {
                log.info(s"set max timeout between checks to $max_timeout_min min")
                max_timeout_between_check_seconds = max_timeout_min*60
              } else {
                log.error(s"wrong max-timeout-min param: $max_timeout_min, must be integer above zero")
              }
              current_command_number += 1
              commandsCheck()
            case "goto" =>
              val goto = str2intOrDefault(command_split(1), -1)
              if(goto >= 1 && goto != current_command_number) {
                log.info(s"going to $goto command")
                current_command_number = goto-1
              } else {
                log.error(s"wrong goto command: $goto")
                current_command_number += 1
              }
              commandsCheck()
            case "build-mine" =>
              val mine = command_split(1)
              val result = ResourcesParser.buildMine(mine)
              if(result) {
                if(command_split.length > 2 && "mail" == command_split(2)) {
                  sendMailSimple(gmail_login, gmail_pass,
                    "started to build mine",
                    s"started to build mine $mine")
                }
                current_command_number += 1
              }
              scheduleNextCheck()
            case "build-station" =>
              val station = command_split(1)
              val result = StationParser.buildStation(station)
              if(result) {
                if(command_split.length > 2 && "mail" == command_split(2)) {
                  sendMailSimple(gmail_login, gmail_pass,
                    "started to build station",
                    s"started to build station $station")
                }
                current_command_number += 1
              }
              scheduleNextCheck()
            case "research" =>
              val tech = command_split(1)
              val result = ResearchParser.research(tech)
              if(result) {
                if(command_split.length > 2 && "mail" == command_split(2)) {
                  sendMailSimple(gmail_login, gmail_pass,
                    "started to research tech",
                    s"started to research tech $tech")
                }
                current_command_number += 1
              }
              scheduleNextCheck()
            case "quit" =>
              log.info("going to shutdown")
              sendMailSimple(gmail_login, gmail_pass,
                "going to shutdown",
                "going to shutdown")
              context.system.shutdown()
            case x =>
              log.warn(s"unknown command: $command")
              current_command_number += 1
              commandsCheck()
          }
        } else if(command_split.length > 0) {
          command_split(0) match {
            case "quit" =>
              log.info("going to shutdown")
              sendMailSimple(gmail_login, gmail_pass,
                "going to shutdown",
                "going to shutdown")
              context.system.shutdown()
            case x =>
              log.warn(s"unknown command: $command")
              current_command_number += 1
              commandsCheck()
          }
        } else {
          log.warn(s"unknown command: $command")
          current_command_number += 1
          commandsCheck()
        }
      } else {
        log.warn(s"no command on line ${current_command_number+1}")
        scheduleNextCheck()
      }
    }
  }

  private def loadCommands:Array[String] = {
    val commands_file = new java.io.File("commands")
    if(!commands_file.exists()) Array()
    else {
      try {
        io.Source.fromFile("commands").getLines().toArray
      } catch {
        case t:Throwable =>
          log.error(s"failed to load existing commands file: $t")
          Array()
      }
    }
  }

  private def attackCheck() {
    if(OverviewParser.under_attack) {
      log.info("found ongoing attack!")
      sendMailSimple(gmail_login, gmail_pass,
        "ongoing attack!",
        "ongoing attack!")
    }
  }

  private def newMessagesCheck() {
    if(OverviewParser.new_messages.info > OverviewParser.previous_new_messages) {
      val diff = OverviewParser.new_messages.info - OverviewParser.previous_new_messages
      log.info(s"found new messages: $diff")
      sendMailSimple(gmail_login, gmail_pass,
        "new messages",
        s"new _messages: $diff\noverall messages: ${OverviewParser.new_messages.info}")
    }
  }

  def receive = {
    case EnterSite =>
      conn.executeGet("http://ru.ogame.gameforge.com")
      val random_wait_time = (math.random*10+1).toLong
      log.info(s"performing login in ${duration2str(random_wait_time)}")
      context.system.scheduler.scheduleOnce(delay = random_wait_time.seconds) {
        self ! PerformLogin
      }
    case PerformLogin =>
      val login_data = new JSONObject()
      login_data.put("kid", "")
      login_data.put("uni", "s118-ru.ogame.gameforge.com")
      login_data.put("login", "nulli")
      login_data.put("pass", "lienajava")
      conn.addPostData(login_data)
      //conn.addHeader("Host", "ru.ogame.gameforge.com")
      conn.addHeader("Origin", "http://ru.ogame.gameforge.com")
      conn.addHeader("Referer", "http://ru.ogame.gameforge.com/")
      conn.executePost("http://ru.ogame.gameforge.com/main/login")
      OverviewParser.parse(conn.currentHtml)
      if(OverviewParser.isEmpty) {
        log.error("failed to login")
        log.error(s"current html:\n${conn.currentHtml}")
        sendMailSimple(gmail_login, gmail_pass,
          "failed to login!",
          "failed to login!")
        context.system.shutdown()
      } else {
        log.info("logged in")
        is_logged_in = true
        log.info(s"Resources: m ${OverviewParser.metal.info} : c ${OverviewParser.crystal.info} : d ${OverviewParser.deuterium.info} : e ${OverviewParser.energy.info}")
        attackCheck()
        newMessagesCheck()
        commandsCheck()
      }
    case Overview =>
      if(!is_logged_in) {
        log.info("not logged in, entering...")
        self ! EnterSite
      } else {
        conn.executeGet(s"http://$uni/game/index.php?page=overview")
        log.info("performing check")
        OverviewParser.parse(conn.currentHtml)
        if(OverviewParser.isEmpty) {
          is_logged_in = false
          log.error("failed to obtain overview data. Trying to relogin")
          self ! EnterSite
        } else {
          log.info(s"Resources: m ${OverviewParser.metal.info} : c ${OverviewParser.crystal.info} : d ${OverviewParser.deuterium.info} : e ${OverviewParser.energy.info}")
          attackCheck()
          newMessagesCheck()
          commandsCheck()
        }
      }
    case Resources =>
      conn.executeGet(s"http://$uni/game/index.php?page=resources")
      ResourcesParser.parse(conn.currentHtml)
      if(ResourcesParser.login_indicator) {
        context.system.scheduler.scheduleOnce(delay = 5.seconds) {
          self ! randomPage
        }
      } else {
        log.error("it seems we need to relogin")
        self ! EnterSite
      }
    case Station =>
      conn.executeGet(s"http://$uni/game/index.php?page=station")
      StationParser.parse(conn.currentHtml)
      if(StationParser.login_indicator) {
        context.system.scheduler.scheduleOnce(delay = 5.seconds) {
          self ! randomPage
        }
      } else {
        log.error("it seems we need to relogin")
        self ! EnterSite
      }
    case Research =>
      conn.executeGet(s"http://$uni/game/index.php?page=research")
      ResearchParser.parse(conn.currentHtml)
      if(ResearchParser.login_indicator) {
        context.system.scheduler.scheduleOnce(delay = 5.seconds) {
          self ! randomPage
        }
      } else {
        log.error("it seems we need to relogin")
        self ! EnterSite
      }
    case Shipyard =>
      conn.executeGet(s"http://$uni/game/index.php?page=shipyard")
      ShipyardParser.parse(conn.currentHtml)
      if(ShipyardParser.login_indicator) {
        context.system.scheduler.scheduleOnce(delay = 5.seconds) {
          self ! randomPage
        }
      } else {
        log.error("it seems we need to relogin")
        self ! EnterSite
      }
    case Defense =>
      conn.executeGet(s"http://$uni/game/index.php?page=defense")
      DefenseParser.parse(conn.currentHtml)
      if(DefenseParser.login_indicator) {
        context.system.scheduler.scheduleOnce(delay = 5.seconds) {
          self ! randomPage
        }
      } else {
        log.error("it seems we need to relogin")
        self ! EnterSite
      }
    case Fleet =>
      conn.executeGet(s"http://$uni/game/index.php?page=fleet1")
      FleetParser.parse(conn.currentHtml)
      if(FleetParser.login_indicator) {
        context.system.scheduler.scheduleOnce(delay = 5.seconds) {
          self ! randomPage
        }
      } else {
        log.error("it seems we need to relogin")
        self ! EnterSite
      }
  }
}
