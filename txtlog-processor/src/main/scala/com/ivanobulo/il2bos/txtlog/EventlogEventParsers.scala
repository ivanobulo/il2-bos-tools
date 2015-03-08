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
      while(!(rest.atEnd || isSuccess(logItemId.apply(rest)))) {
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

  def eventN[T <: LogEvent](x: Int, p: => Parser[T]) = eventType(x) ~> p

  def gameDate = "GDate" ~ COLUMN ~> gameLocalDate

  def gameTime = "GTime" ~ COLUMN ~> gameLocalTime

  def missionFile = "MFile" ~ COLUMN ~> allCharsBeforeNextValue

  def intValue(key:String) = literal(key) ~ COLUMN ~> decimalNumber ^^ {_.toInt}

  def doubleValue(key:String) = literal(key) ~ COLUMN ~> decimalNumber ^^ {_.toDouble}

  def stringValue(key:String) = literal(key) ~ COLUMN ~> allCharsBeforeNextValue

  def missionId = "MID" ~ COLUMN ~> opt(decimalNumber) ^^ { _.map(_.toInt) }

  def gameType = intValue("GType")

  def counters = ("CNTRS" ~ COLUMN) ~> intMap

  def mods = intValue("MODS")

  def preset = intValue("PRESET")

  def settingsFlags = stringValue("SETTS")

  def aqmId = intValue("AQMID")

  def aid = intValue("AID")

  def tid = intValue("TID")
  def plid = intValue("PLID")
  def pid = intValue("PID")
  def bul = intValue("BUL")
  def sh = intValue("SH")
  def bomb = intValue("BOMB")
  def rct = intValue("RCT")

  def ammo = stringValue("AMMO")

  def damage = doubleValue("DMG")

  def pos = "POS" ~> position

  def someEvent = eventN(0, missionStart) | eventN(1, hitEvent) | eventN(2, damageEvent) | eventN(3, killEvent) |
    eventN(4, playerAmmoEvent)

  def event:Parser[(Long, LogEvent)] = eventTime ~ someEvent ^^ {
    case time ~ e => (time, e)
  }

  def missionStart:Parser[LogEvent] =
    gameDate ~ gameTime ~ missionFile ~ missionId ~ gameType ~ counters ~ settingsFlags ~ mods ~ preset ~ aqmId ^^ {
      case date ~ time ~ map ~ mid ~ gameType ~ countryCounters ~ settingsFlags ~ mods ~ preset ~ aqmId =>
        MissionStartEvent(date, time, map, mid)
    }

  def hitEvent = ammo ~ aid ~ tid ^^ {
    case ammo ~ aid ~ tid => HitEvent(ammo, aid, tid)
  }

  def damageEvent = damage ~ aid ~ tid ~ pos ^^ {
    case damage ~ aid ~ tid ~ pos => DamageEvent(damage, aid, tid, pos)
  }

  def killEvent = aid ~ tid ~ pos ^^ {
    case aid ~ tid ~ pos => KillEvent(aid, tid, pos)
  }

  def playerAmmoEvent = plid ~ pid ~ bul ~ sh ~ bomb ~ rct ~ position ^^ {
    case plid ~ pid ~ bul ~ sh ~ bomb ~ rct ~ position => PlayerAmmoEvent(plid, pid, bul, sh, bomb, rct, position)
  }
}

trait LogEvent

case class HitEvent(ammo:String, attackerId:Int, targetId:Int) extends LogEvent

case class DamageEvent(damage:Double, attackerId:Int, targetId:Int, pos: Position) extends LogEvent

case class KillEvent(attackerId:Int, targetId:Int, pos: Position) extends LogEvent

case class PlayerAmmoEvent(playerId:Int, planeId:Int, bullets:Int, shells:Int, bombs:Int,  rockets:Int,  pos: Position) extends LogEvent

case class MissionStartEvent(gameDate: LocalDate, gameTime: LocalTime, map: String, missionId: Option[Int]) extends LogEvent

case class TimestampedEvent(time: Long, event: LogEvent)

case class Position(x: Double, y: Double, z: Double)

case class Area(positions: List[Position])
