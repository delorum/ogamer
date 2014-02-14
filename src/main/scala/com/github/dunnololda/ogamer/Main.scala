package com.github.dunnololda.ogamer

import com.github.dunnololda.conn.Conn
import org.json.JSONObject
import com.github.dunnololda.cli.Imports._
import akka.actor.{Props, Actor, ActorSystem}
import concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import com.github.dunnololda.ogamer.htmlparsers._

object Main extends Cli {
  programDescription = s"Ogame Controller v$appVersion"
  commandLineArgsAndParse(
    ("u", "uni", "ogame universe to log in", true, true),
    ("l", "login", "ogame user login", true, true),
    ("p", "pass", "ogame user password", true, true),
    ("g", "gmail-login", "gmail login to send notifications to", true, true),
    ("gp", "gmail-pass", "gmail password", true, true)
  )

  val uni         = stringProperty("uni")
  val login       = stringProperty("login")
  val pass        = stringProperty("pass")
  val gmail_login = stringProperty("gmail-login")
  val gmail_pass  = stringProperty("gmail-pass")

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

case object OverviewPage
case object ResourcesPage
case object StationPage
case object ResearchPage
case object ShipyardPage
case object DefensePage
case object Fleet1Page

class Master(login:String, pass:String, gmail_login:String, gmail_pass:String)(implicit uni:String) extends Actor {
  private val log = MySimpleLogger(this.getClass.getName)

  private val all_pages = List(OverviewPage, ResourcesPage, StationPage, Research, ShipyardPage, DefensePage, Fleet1Page)
  def randomPage = {
    if(math.random < 0.3) OverviewPage
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
    val commands = CommandsParser.loadCommands

    if(commands.isEmpty) {
      log.info("no commands found")
      scheduleNextCheck()
    } else {
      if(current_command_number >= 0 && current_command_number < commands.length) {
        val command = commands(current_command_number)
        log.info(s"current command: ${current_command_number+1} : $command")
        command match {
          case SendFleet(mission, fleet, resources, target_planet, mail) =>
            val result = Fleet1Parser.sendFleet(mission, fleet, resources, (2,104,10), target_planet)
            if(result) {
              if(mail) {
                sendMailSimple(gmail_login, gmail_pass,
                  "sent fleet",
                  s"sent fleet:\n" +
                    s"from planet: 2:104:10\n" +
                    s"fleet: ${fleet.map(x => s"${x._1}:${x._2}").mkString(", ")}\n" +
                    s"mission: $mission\n" +
                    s"resources: m:${resources._1} c:${resources._2} d:${resources._3}\n" +
                    s"target planet: ${target_planet._1}:${target_planet._2}:${target_planet._3}"
                )
              }
              current_command_number += 1
            }
            scheduleNextCheck()
          case BuildShip(ship, amount, mail) =>
            val result = ShipyardParser.buildShip(ship, amount)
            if(result) {
              if(mail) {
                sendMailSimple(gmail_login, gmail_pass,
                  "started to build ship",
                  s"started to build ship $ship")
              }
              current_command_number += 1
            }
            scheduleNextCheck()
          case BuildDefense(defense, amount, mail) =>
            val result = DefenseParser.buildDefense(defense, amount)
            if(result) {
              if(mail) {
                sendMailSimple(gmail_login, gmail_pass,
                  "started to build defense",
                  s"started to build defense $defense")
              }
              current_command_number += 1
            }
            scheduleNextCheck()
          case Limits(metal_limit, crystal_limit, deuterium_limit, mail) =>
            val need_to_reach_limit = metal_limit != 0 || crystal_limit != 0 || deuterium_limit != 0
            if(need_to_reach_limit) {
              val metal_limit_reached = metal_limit == 0 || OverviewParser.metal.info >= metal_limit
              val crystal_limit_reached = crystal_limit == 0 || OverviewParser.crystal.info >= crystal_limit
              val deuterium_limit_reached = deuterium_limit == 0 || OverviewParser.deuterium.info >= deuterium_limit
              if(metal_limit_reached && crystal_limit_reached && deuterium_limit_reached) {
                log.info(s"limits reached: ${OverviewParser.metal.info}/$metal_limit : ${OverviewParser.crystal.info}/$crystal_limit : ${OverviewParser.deuterium.info}/$deuterium_limit")
                if(mail) {
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
          case MaxTimeoutMin(min) =>
            if(min > 0) {
                log.info(s"set max timeout between checks to $min min")
                max_timeout_between_check_seconds = min*60
              } else {
                log.error(s"wrong max-timeout-min param: $min, must be integer above zero")
              }
              current_command_number += 1
              commandsCheck()
          case Goto(goto) =>
            if(goto >= 1 && goto != current_command_number) {
              log.info(s"going to $goto command")
              current_command_number = goto-1
            } else {
              log.error(s"wrong goto command: $goto")
              current_command_number += 1
            }
            commandsCheck()
          case BuildMine(mine, mail) =>
            val result = ResourcesParser.buildMine(mine)
            if(result) {
              if(mail) {
                sendMailSimple(gmail_login, gmail_pass,
                  "started to build mine",
                  s"started to build mine $mine")
              }
              current_command_number += 1
            }
            scheduleNextCheck()
          case BuildStation(station, mail) =>
            val result = StationParser.buildStation(station)
            if(result) {
              if(mail) {
                sendMailSimple(gmail_login, gmail_pass,
                  "started to build station",
                  s"started to build station $station")
              }
              current_command_number += 1
            }
            scheduleNextCheck()
          case Research(tech, mail) =>
            val result = ResearchParser.research(tech)
            if(result) {
              if(mail) {
                sendMailSimple(gmail_login, gmail_pass,
                  "started to research tech",
                  s"started to research tech $tech")
              }
              current_command_number += 1
            }
            scheduleNextCheck()
          case Quit =>
            log.info("going to shutdown")
            sendMailSimple(gmail_login, gmail_pass,
              "going to shutdown",
              "going to shutdown")
            context.system.shutdown()
          case UnknownCommand(unknown_command) =>
            log.warn(s"unknown command: $unknown_command")
            current_command_number += 1
            commandsCheck()
          case x =>
            log.warn(s"unknown command: $x")
            current_command_number += 1
            commandsCheck()
        }
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
    case OverviewPage =>
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
    case ResourcesPage =>
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
    case StationPage =>
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
    case ResearchPage =>
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
    case ShipyardPage =>
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
    case DefensePage =>
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
    case Fleet1Page =>
      conn.executeGet(s"http://$uni/game/index.php?page=fleet1")
      Fleet1Parser.parse(conn.currentHtml)
      if(Fleet1Parser.login_indicator) {
        context.system.scheduler.scheduleOnce(delay = 5.seconds) {
          self ! randomPage
        }
      } else {
        log.error("it seems we need to relogin")
        self ! EnterSite
      }
  }
}
