package com.github.dunnololda.ogamer

import scala.util.parsing.combinator._

object ParserTest extends App with JavaTokenParsers {
  def flightInfoParser:Parser[(Int,Int,Int)] = "\"hostile\""~":"~wholeNumber~","~"\"neutral\""~":"~wholeNumber~","~"\"friendly\""~":"~wholeNumber~".*".r ^^ {
    case "\"hostile\""~":"~hostile~","~"\"neutral\""~":"~neutral~","~"\"friendly\""~":"~friendly~s => (hostile.toInt, neutral.toInt, friendly.toInt)
  }

  val str = "\"hostile\" : 0, \"neutral\" :0, \"friendly\": 1, \"eventTime\":40482,\"eventText\":\"\\u0410\\u0442\\u0430\\u043a\\u0430"
  println(parseAll(flightInfoParser, str))
}
