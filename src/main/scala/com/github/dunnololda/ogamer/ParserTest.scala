package com.github.dunnololda.ogamer

import scala.util.parsing.combinator._

object ParserTest extends App with JavaTokenParsers {
  def flightInfoParser:Parser[(Int,Int,Int)] = "\"hostile\""~":"~wholeNumber~","~"\"neutral\""~":"~wholeNumber~","~"\"friendly\""~":"~wholeNumber~".*".r ^^ {
    case "\"hostile\""~":"~hostile~","~"\"neutral\""~":"~neutral~","~"\"friendly\""~":"~friendly~s => (hostile.toInt, neutral.toInt, friendly.toInt)
  }

  val str = "{reloadEventbox({\"hostile\":0,\"neutral\":0,\"friendly\":1,\"eventTime\":46,\"eventText\":\"\\u0428\\u043f\\u0438\\u043e\\u043d\\u0430\\u0436 (\\u0412)\"});}"
  println(parseAll(flightInfoParser, str.drop(str.indexOf("reloadEventbox({")).drop("reloadEventbox({".length).takeWhile(_ != '}')))
}
