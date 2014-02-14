package com.github.dunnololda.ogamer

object ParserTest extends App {
  val str = "send transport light-interceptor:2,small-transport:3 1000:2000:3000 2:104:8"
  println(CommandsParser.parseAll(CommandsParser.commandParser, str))
}
