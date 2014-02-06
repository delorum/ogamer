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
    ("u", "uni", "universe to log in", true, true),
    ("l", "login", "login", true, true),
    ("p", "pass", "password", true, true)
  )

  val uni = stringProperty("uni")
  val login = stringProperty("login")
  val pass = stringProperty("pass")

  private val system = ActorSystem("ogame")
  system.actorOf(Props(new Master(uni, login, pass)), name = "master")

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

class Master(uni:String, login:String, pass:String) extends Actor {
  private val log = MySimpleLogger(this.getClass.getName)

  private val max_timeout_between_check_seconds = 20*60   // in seconds

  private val conn = new Conn
  private var is_logged_in = false

  override def preStart() {
    log.info(s"starting actor ${self.path.toString}")
    self ! Overview
  }

  override def postStop() {
    log.info(s"${self.path.toString} died!")
  }

  private def overviewCheck() {
    if(OverviewParser.isEmpty) {
      log.error("failed to obtain overview data. Trying to relogin")
      self ! EnterSite
    } else {
      log.info(s"Resources: m ${OverviewParser.metal} : c ${OverviewParser.crystal} : d ${OverviewParser.deuterium}")
      val limits_file = new java.io.File("limits")
      if(limits_file.exists()) {
        val lines = io.Source.fromFile("limits").getLines()
        if(lines.hasNext) {
          val limits = lines.next().split(":")
          if(limits.length == 3) {
            val metal_limit = str2intOrDefault(limits(0), 0)
            val crystal_limit = str2intOrDefault(limits(1), 0)
            val deuterium_limit = str2intOrDefault(limits(2), 0)
            val need_to_reach_limit = metal_limit != 0 || crystal_limit != 0 || deuterium_limit != 0
            if(need_to_reach_limit) {
              val metal_limit_reached = metal_limit == 0 || OverviewParser.metal >= metal_limit
              val crystal_limit_reached = crystal_limit == 0 || OverviewParser.crystal >= crystal_limit
              val deuterium_limit_reached = deuterium_limit == 0 || OverviewParser.deuterium >= deuterium_limit
              if(metal_limit_reached && crystal_limit_reached && deuterium_limit_reached) {
                log.info(s"limit reached: ${OverviewParser.metal}/$metal_limit : ${OverviewParser.crystal}/$crystal_limit : ${OverviewParser.deuterium}/$deuterium_limit")
                sendMailSimple("ogameinformer7@gmail.com", "ogame report", s"limit reached:\n${OverviewParser.metal}/$metal_limit : ${OverviewParser.crystal}/$crystal_limit : ${OverviewParser.deuterium}/$deuterium_limit")
                limits_file.delete()
              } else {
                log.info(s"limit not reached: ${OverviewParser.metal}/$metal_limit : ${OverviewParser.crystal}/$crystal_limit : ${OverviewParser.deuterium}/$deuterium_limit")
              }
            } else {
              log.error(s"limits file exists but no limits set: $limits")
            }
          } else {
            log.error(s"limits file exists but no limits set: $limits")
          }
        } else {
          log.error(s"limits file exists but has no content")
        }
      } else {
        log.info("no limits set")
      }
      val random_wait_time = (math.random*max_timeout_between_check_seconds+1).toLong
      log.info(s"performing next check in ${duration2str(random_wait_time)}")
      context.system.scheduler.scheduleOnce(delay = random_wait_time.seconds) {
        self ! Overview
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
        sendMailSimple("ogameinformer7@gmail.com", "ogame report", "failed to login!")
        context.system.shutdown()
      }
  }
}
