package com.github.dunnololda.ogamer

import com.github.dunnololda.conn.Conn
import org.json.JSONObject
import com.github.dunnololda.cli.Imports._
import akka.actor.{Props, Actor, ActorSystem}
import concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import com.github.dunnololda.ogamer.htmlparsers._
import scala.collection.mutable

class CommandNumber(var current:Int) {
  val previous:mutable.Stack[Int] = mutable.Stack[Int](current)

  def showCurrent = current+1
  def showPrevious = previous.head+1
  def nextNumber() {
    previous.push(current)
    current += 1
  }
  def goto(goto:Int) {
    previous.push(current)
    current = goto
  }

  def returnBack() {
    current = previous.pop()
  }
}

object Main extends Cli {
  programDescription = s"Ogame Controller v$appVersion"
  commandLineArgsAndParse(
    ("u", "uni", "ogame universe to log in", true, true),
    ("l", "login", "ogame user login", true, true),
    ("p", "pass", "ogame user password", true, true),
    ("g", "gmail-login", "gmail login to send notifications to", true, true),
    ("gp", "gmail-pass", "gmail password", true, true),
    ("c", "commands", "path to file with commands for program. 'commands' by default", true, false)
  )

  val uni         = stringProperty("uni")
  val login       = stringProperty("login")
  val pass        = stringProperty("pass")
  val gmail_login = stringProperty("gmail-login")
  val gmail_pass  = stringProperty("gmail-pass")
  val commands_filename = property("commands", "commands")

  private val system = ActorSystem("ogame")
  system.actorOf(Props(new Master(login, pass, gmail_login, gmail_pass, commands_filename)(uni)), name = "master")

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

class Master(login:String, pass:String, gmail_login:String, gmail_pass:String, commands_filename:String)(implicit uni:String) extends Actor {
  private val log = MySimpleLogger(this.getClass.getName)

  private val all_pages = List(OverviewPage, ResourcesPage, StationPage, ResearchPage, ShipyardPage, DefensePage, Fleet1Page)
  def randomPage = {
    if(math.random < 0.3) OverviewPage
    else all_pages((math.random*all_pages.length).toInt)
  }

  private var max_timeout_between_check_seconds = 5*60   // 5 minutes in seconds
  private val command_number = new CommandNumber(0)
  private var send_mail_on_new_messages = true

  private var current_planet = ""

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
    val commands = CommandsParser.loadCommands(commands_filename)
    if(commands.isEmpty) {
      log.info("no commands found")
      scheduleNextCheck()
    } else {
      if(command_number.current >= 0 && command_number.current < commands.length) {
        val command = commands(command_number.current)
        log.info(s"current command: ${command_number.showCurrent} : $command")
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
              command_number.nextNumber()
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
              command_number.nextNumber()
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
              command_number.nextNumber()
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
                command_number.nextNumber()
                commandsCheck()
              } else {
                log.info(s"limits not reached: ${OverviewParser.metal.info}/$metal_limit : ${OverviewParser.crystal.info}/$crystal_limit : ${OverviewParser.deuterium.info}/$deuterium_limit")
                scheduleNextCheck()
              }
            } else {
              log.error(s"no limits set: $command")
              command_number.nextNumber()
              commandsCheck()
            }
          case MaxTimeoutMin(min) =>
            if(min > 0) {
                log.info(s"set max timeout between checks to $min min")
                max_timeout_between_check_seconds = min*60
              } else {
                log.error(s"wrong max-timeout-min param: $min, must be integer above zero")
              }
              command_number.nextNumber()
              commandsCheck()
          case Goto(goto) =>
            if(goto >= 1 && goto != command_number.current) {
              log.info(s"going to $goto command")
              command_number.goto(goto-1)
            } else {
              log.error(s"wrong goto command: $goto")
              command_number.nextNumber()
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
              command_number.nextNumber()
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
              command_number.nextNumber()
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
              command_number.nextNumber()
            }
            scheduleNextCheck()
          case FleetLimits(fleet, maybe_else_goto, mail) =>
            val (result, str_result) = Fleet1Parser.checkLimits(fleet)
            if(result) {
              log.info(s"fleet limits reached: $str_result")
              if(mail) {
                sendMailSimple(gmail_login, gmail_pass,
                  "fleet limits reached",
                  s"fleet limits reached: $str_result")
              }
              command_number.nextNumber()
              commandsCheck()
            } else {
              log.info(s"fleet limits not reached: $str_result")
              maybe_else_goto match {
                case Some(else_goto) =>
                  if(else_goto >= 1) {
                    log.info(s"going to $else_goto command")
                    command_number.goto(else_goto-1)
                  } else {
                    log.error(s"wrong goto command: $else_goto")
                    command_number.nextNumber()
                  }
                  commandsCheck()
                case None =>
                  scheduleNextCheck()
              }
            }

          case WaitFleetReturn =>
            val result = OverviewParser.noSelfFlightActivity
            if(result) {
              log.info("no self flight activity detected")
              command_number.nextNumber()
              commandsCheck()
            } else {
              log.info(s"self flight activity: ${OverviewParser.friendly_flights} mission(s)")
              scheduleNextCheck()
            }
          case SelfFlightsLE(amount) =>
            val result = OverviewParser.friendly_flights <= amount
            if(result) {
              log.info(s"self flight activity: ${OverviewParser.friendly_flights} mission(s), less or equal then required amount: $amount")
              command_number.nextNumber()
              commandsCheck()
            } else {
              log.info(s"self flight activity: ${OverviewParser.friendly_flights} mission(s), more then required: $amount")
              scheduleNextCheck()
            }
          case Quit(mail) =>
            log.info("going to shutdown")
            if(mail) {
              sendMailSimple(gmail_login, gmail_pass,
                "going to shutdown",
                "going to shutdown")
            }
            context.system.shutdown()
          case MailNewMessages(send_mail:Boolean) =>
            if(send_mail) {
              log.info("send mail on new messages")
            } else {
              log.info("don't send mail on new messages")
            }
            send_mail_on_new_messages = send_mail
            command_number.nextNumber()
            commandsCheck()
          case Return =>
            if(command_number.previous.nonEmpty) {
              log.info(s"return to ${command_number.showPrevious} command")
              command_number.returnBack()
            } else {
              log.error(s"wrong return command: nowhere to return")
              command_number.nextNumber()
            }
            commandsCheck()
          case LogHtml =>
            log.info("=========================== HTML LOG ===========================")
            log.info(conn.currentHtml)
            log.info("================================================================")
            command_number.nextNumber()
            commandsCheck()
          case SetPlanet(planet) =>
            val result = OverviewParser.setPlanet(planet)
            if(result) {
              current_planet = planet
            }
            command_number.nextNumber()
            commandsCheck()
          case EmptyString =>
            command_number.nextNumber()
            commandsCheck()
          case Comment =>
            command_number.nextNumber()
            commandsCheck()
          case UnknownCommand(unknown_command) =>
            log.warn(s"unknown command: $unknown_command")
            command_number.nextNumber()
            commandsCheck()
          case x =>
            log.warn(s"unknown command: $x")
            command_number.nextNumber()
            commandsCheck()
        }
      } else {
        log.info(s"no command on line ${command_number.showCurrent}")
        scheduleNextCheck()
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
      log.info(s"found new messages: $diff, overall messages: ${OverviewParser.new_messages.info}")
      if(send_mail_on_new_messages) {
        sendMailSimple(gmail_login, gmail_pass,
          "new messages",
          s"new _messages: $diff\noverall messages: ${OverviewParser.new_messages.info}")
      }
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
      login_data.put("uni", uni)
      login_data.put("login", login)
      login_data.put("pass", pass)
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
        if(current_planet != "") {
          log.info(s"setting current planet $current_planet")
          OverviewParser.setPlanet(current_planet)
        }
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
        context.system.scheduler.scheduleOnce(delay = (math.random*5+1).toLong.seconds) {
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
        context.system.scheduler.scheduleOnce(delay = (math.random*5+1).toLong.seconds) {
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
        context.system.scheduler.scheduleOnce(delay = (math.random*5+1).toLong.seconds) {
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
        context.system.scheduler.scheduleOnce(delay = (math.random*5+1).toLong.seconds) {
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
        context.system.scheduler.scheduleOnce(delay = (math.random*5+1).toLong.seconds) {
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
        context.system.scheduler.scheduleOnce(delay = (math.random*5+1).toLong.seconds) {
          self ! randomPage
        }
      } else {
        log.error("it seems we need to relogin")
        self ! EnterSite
      }
    case x =>
      log.warn(s"unknown message: $x")
      context.system.scheduler.scheduleOnce(delay = (math.random*5+1).toLong.seconds) {
        self ! randomPage
      }
  }
}
