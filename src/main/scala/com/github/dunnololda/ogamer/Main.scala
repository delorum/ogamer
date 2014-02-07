package com.github.dunnololda.ogamer

import com.github.dunnololda.conn.Conn
import org.json.JSONObject
import scala.collection.JavaConversions._
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
  system.actorOf(Props(new Master(uni, login, pass, gmail_login, gmail_pass)), name = "master")

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run() {
      system.shutdown()
      system.awaitTermination()
    }
  })
}

case object Overview
case object EnterSite
case object PerformLogin

class Master(uni:String, login:String, pass:String, gmail_login:String, gmail_pass:String) extends Actor {
  private val log = MySimpleLogger(this.getClass.getName)

  private var max_timeout_between_check_seconds = 5*60   // in seconds
  private var current_command_number = 0

  private implicit val conn = new Conn
  private var is_logged_in = false

  override def preStart() {
    log.info(s"starting actor ${self.path.toString}")
    self ! Overview
  }

  override def postStop() {
    log.info(s"${self.path.toString} died!")
  }

  private def buildMine(mine:String):Boolean = {
    log.info(s"trying to build mine $mine")
    conn.executeGet(s"http://$uni/game/index.php?page=resources")
    ResourcesParser.parse(conn.currentHtml)
    ResourcesParser.build(mine)
  }

  private def buildStation(station:String):Boolean = {
    log.info(s"trying to build mine $station")
    conn.executeGet(s"http://$uni/game/index.php?page=station")
    StationParser.parse(conn.currentHtml)
    StationParser.build(station)
  }

  private def research(tech:String):Boolean = {
    log.info(s"trying to research $tech")
    conn.executeGet(s"http://$uni/game/index.php?page=research")
    ResearchParser.parse(conn.currentHtml)
    ResearchParser.research(tech)
  }

  private def scheduleNextCheck() {
    val random_wait_time = (math.random*max_timeout_between_check_seconds+1).toLong
    log.info(s"performing next check in ${duration2str(random_wait_time)}")
    context.system.scheduler.scheduleOnce(delay = random_wait_time.seconds) {
      self ! Overview
    }
  }

  private def overviewCheck() {
    if(OverviewParser.isEmpty) {
      log.error("failed to obtain overview data. Trying to relogin")
      self ! EnterSite
    } else {
      log.info(s"Resources: m ${OverviewParser.metal.info} : c ${OverviewParser.crystal.info} : d ${OverviewParser.deuterium.info} : e ${OverviewParser.energy.info}")
      val commands = loadCommands
      if(commands.isEmpty) {
        log.info("no commands found")
        scheduleNextCheck()
      } else {
        if(current_command_number >= 0 && current_command_number < commands.length) {
          val command = commands(current_command_number)
          log.info(s"current command: $current_command_number : $command")
          val command_split = command.split(" ")
          if(command_split.length > 1) {
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
                      overviewCheck()
                    } else {
                      log.info(s"limits not reached: ${OverviewParser.metal.info}/$metal_limit : ${OverviewParser.crystal.info}/$crystal_limit : ${OverviewParser.deuterium.info}/$deuterium_limit")
                      scheduleNextCheck()
                    }
                  } else {
                    log.error(s"no limits set: $command")
                    current_command_number += 1
                    overviewCheck()
                  }
                } else {
                  log.error(s"no limits set: $command")
                  current_command_number += 1
                  overviewCheck()
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
                overviewCheck()
              case "goto" =>
                val goto = str2intOrDefault(command_split(1), -1)
                if(goto >= 0 && goto != current_command_number) {
                  log.info(s"going to $goto command")
                  current_command_number = goto
                } else {
                  log.error(s"wrong goto command: $goto")
                  current_command_number += 1
                }
                overviewCheck()
              case "build-mine" =>
                val mine = command_split(1)
                val result = buildMine(mine)
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
                val result = buildStation(station)
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
                val result = research(tech)
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
                context.system.shutdown()
              case x =>
                log.warn(s"unknown command: $command")
                current_command_number += 1
                overviewCheck()
            }
          } else {
            log.warn(s"unknown command: $command")
            current_command_number += 1
            overviewCheck()
          }
        } else {
          log.warn(s"no command on line $current_command_number")
          scheduleNextCheck()
        }
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

  def receive = {
    case Overview =>
      if(!is_logged_in) {
        log.info("not logged in, entering...")
        self ! EnterSite
      } else {
        log.info("performing check")
        conn.executeGet(s"http://$uni/game/index.php?page=overview")
        OverviewParser.parse(conn.currentHtml)
        overviewCheck()
      }
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
      if(OverviewParser.nonEmpty) {
        log.info("logged in")
        is_logged_in = true
        overviewCheck()
      } else {
        log.error("failed to login")
        log.error(s"current html:\n${conn.currentHtml}")
        sendMailSimple(gmail_login, gmail_pass,
          "failed to login!",
          "failed to login!")
        context.system.shutdown()
      }
  }
}
