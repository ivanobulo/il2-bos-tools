package com.ivanobulo.il2bos.txtlog

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalTime}
import java.util.UUID

import scala.util.parsing.combinator.JavaTokenParsers

class EventlogEventParsers extends JavaTokenParsers {
  lazy val gameDateFormat = DateTimeFormatter.ofPattern("yyyy.M.d")
  lazy val gameTimeFormat = DateTimeFormatter.ofPattern("HH:m:s")

  def COMMA = ","

  def COLUMN = ":"

  def LP = "("

  def RP = ")"

  def coordinate = decimalNumber

  def logItemId: Parser[String] =
    """[A-Z][A-Za-z]+:""".r

  /**
   * looks forward for end of input or next 'logItemId'
   */
  def allCharsBeforeNextValue:Parser[String] = new Parser[String] {
    def isSuccess(result: ParseResult[String]) = result match {
      case f @ Failure(_,_) => false
      case _ => true
    }

    override def apply(in: Input): ParseResult[String] = {
      val result = new StringBuilder()
      var rest = in
      while(!rest.atEnd && !isSuccess(logItemId.apply(rest))) {
        result.append(rest.first)
        rest = rest.rest
      }
      Success(result.toString(), rest)
    }
  }

  def position = (LP ~> coordinate) ~ (COMMA ~> coordinate) ~ (COMMA ~> coordinate) <~ RP ^^ {
    case x ~ y ~ z => Position(x.toDouble, y.toDouble, z.toDouble)
  }

  def areaBoundary = rep1sep(position, COMMA) ^^ {
    case boundaries => Area(boundaries)
  }

  def numericId = decimalNumber ^^ {
    _.toLong
  }

  def numericIdList: Parser[List[Long]] = rep1sep(numericId, COMMA)

  def gameLocalDate: Parser[LocalDate] = allCharsBeforeNextValue ^^ {
    case timeString => LocalDate.parse(timeString, gameDateFormat)
  }

  def gameLocalTime: Parser[LocalTime] = allCharsBeforeNextValue ^^ {
    case timeString => LocalTime.parse(timeString, gameTimeFormat)
  }

  def uuid = allCharsBeforeNextValue ^^ {
    case x => UUID.fromString(x)
  }

  def intKeyValue = (decimalNumber <~ COLUMN) ~ decimalNumber ^^ {
    case key ~ value => (key.toInt, value.toInt)
  }

  def intMap = repsep(intKeyValue, COMMA) ^^ {
    case list => list.toMap
  }

  def eventTime = ("T" ~ COLUMN) ~> decimalNumber ^^ {
    _.toLong
  }

  def eventType(x: Int) = ("AType" ~ COLUMN) ~> literal(x.toString) ^^ {
    _.toLong
  }

  def event[T <: LogEvent](x: Int, p: => Parser[T]) = (eventTime <~ eventType(x)) ~ p ^^ {
    case time ~ event => (time, event)
  }

  def gameDate = "GDate" ~ COLUMN ~> gameLocalDate

  def gameTime = "GTime" ~ COLUMN ~> gameLocalTime

  def missionFile = "MFile" ~ COLUMN ~> allCharsBeforeNextValue

  def intValue(key:String) = literal(key) ~ COLUMN ~> decimalNumber ^^ {_.toInt}

  def stringValue(key:String) = literal(key) ~ COLUMN ~> allCharsBeforeNextValue

  def missionId = "MID" ~ COLUMN ~> opt(decimalNumber) ^^ { _.map(_.toInt) }

  def gameType = intValue("GType")

  def counters = ("CNTRS" ~ COLUMN) ~> intMap

  def mods = intValue("MODS")

  def preset = intValue("PRESET")

  def settingsFlags = stringValue("SETTS")

  def aqmId = intValue("AQMID")

  def missionStart = event(0, {
    gameDate ~ gameTime ~ missionFile ~ missionId ~ gameType ~ counters ~ settingsFlags ~ mods ~ preset ~ aqmId ^^ {
      case date ~ time ~ map ~ mid ~ gameType ~ countryCounters ~ settingsFlags ~ mods ~ preset ~ aqmId =>
        MissionStartEvent(date, time, map, mid)
    }
  })
}

trait LogEvent

case class FakeEvent(gameDate: LocalDate, gameTime: LocalTime) extends LogEvent

case class MissionStartEvent(gameDate: LocalDate, gameTime: LocalTime, map: String, missionId: Option[Int]) extends LogEvent

case class TimestampedEvent(time: Long, event: LogEvent)

case class Position(x: Double, y: Double, z: Double)

case class Area(positions: List[Position])
