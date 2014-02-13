package com.github.dunnololda

import scala.concurrent.duration.FiniteDuration
import org.apache.commons.mail.{DefaultAuthenticator, HtmlEmail}
import com.github.dunnololda.cli.MySimpleLogger
import concurrent.duration._

package object ogamer {
  private val log = MySimpleLogger(this.getClass.getName)

  case class SmtpConfig(tls : Boolean = false,
                        ssl : Boolean = false,
                        port : Int = 25,
                        host : String,
                        user : String,
                        password: String)

  case class EmailMessage(
                           subject: String,
                           recipient: String,
                           from: String,
                           text: String,
                           html: String,
                           smtpConfig : SmtpConfig,
                           retryOn: FiniteDuration,
                           var deliveryAttempts: Int = 0)

  def sendEmailSync(emailMessage: EmailMessage) {
    val email = new HtmlEmail()
    email.setTLS(emailMessage.smtpConfig.tls)
    email.setSSL(emailMessage.smtpConfig.ssl)
    email.setSmtpPort(emailMessage.smtpConfig.port)
    email.setHostName(emailMessage.smtpConfig.host)
    email.setAuthenticator(new DefaultAuthenticator(
      emailMessage.smtpConfig.user,
      emailMessage.smtpConfig.password
    ))
    email.setHtmlMsg(emailMessage.html)
      .setTextMsg(emailMessage.text)
      .addTo(emailMessage.recipient)
      .setFrom(emailMessage.from)
      .setSubject(emailMessage.subject)
      .send()
  }

  def sendMailSimple(gmail_login:String, gmail_pass:String, subject:String, text:String) {
    log.info(s"sending email:\n$subject\n$text")
    try {
      sendEmailSync(EmailMessage(
        subject = subject,
        recipient = s"$gmail_login@gmail.com",
        from = s"$gmail_login@gmail.com",
        text = " ",
        html = text,
        smtpConfig = SmtpConfig(
          tls = true, ssl = false, host = "smtp.gmail.com", port=  587, user = gmail_login, password = gmail_pass
        ),
        retryOn = 1.minute,
        deliveryAttempts = 5
      ))
    } catch {
      case t:Throwable =>
        log.error(s"failed to send email: $t")
    }
  }

  def str2intOrDefault(str:String, default:Int, without_warning:Boolean = false):Int = {
    try {
      str.trim.replaceAll("\\.", "").toInt
    } catch {
      case t:Throwable =>
        if(!without_warning) log.info(s"failed to parse $str as int: $t")
        default
    }
  }

  def safeDelete(op1:Int, op2:Int):Int = {
    if(op2 != 0) op1/op2 else Int.MaxValue
  }

  def duration2str(duration_sec:Long):String = {
    if (duration_sec <= 0) "0 sec"
    else {
      val b = new StringBuilder
      val sec  = 1l
      val min  = sec*60
      val hour = min*60
      val day  = hour*24

      List(
        (duration_sec/day,      "days"),
        (duration_sec%day/hour, "hours"),
        (duration_sec%hour/min, "min"),
        (duration_sec%min/sec,  "sec")
      ).filter(_._1 > 0).foreach(e => b append e._1 append " " append e._2 append " ")
      b.result().trim
    }
  }
}
