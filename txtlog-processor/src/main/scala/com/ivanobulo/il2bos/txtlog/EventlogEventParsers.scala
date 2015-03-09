package com.ivanobulo.il2bos.txtlog

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalTime}
import java.util.UUID

import com.ivanobulo.il2bos.event._

import scala.util.parsing.combinator.JavaTokenParsers

class EventlogEventParsers extends JavaTokenParsers {
  lazy val gameDateFormat = DateTimeFormatter.ofPattern("yyyy.M.d")
  lazy val gameTimeFormat = DateTimeFormatter.ofPattern("HH:m:s")

  // some literals
  def COMMA = ","
  def COLUMN = ":"
  def LP = "("
  def RP = ")"

  def coordinate = decimalNumber

  def logItemId: Parser[String] = """[A-Z][A-Za-z]*""".r <~ COLUMN

  def allCharsBeforeNextValue:Parser[String] = until(logItemId)

  // dirty backtracking to find everything before 'logItemId'
  def until(p: Parser[_]) = new Parser[String] {
    def isSuccess(result: ParseResult[_]) = result match {
      case f @ Failure(_,_) => false
      case _ => true
    }

    override def apply(in: Input): ParseResult[String] = {
      val result = new StringBuilder()
      var rest = in
      while(!(rest.atEnd || isSuccess(p.apply(rest)))) {
        result.append(rest.first)
        rest = rest.rest
      }
      if (result.isEmpty) Failure("", rest) else Success(result.toString(), rest)
    }
  }

  def id = decimalNumber ^^ { _.toInt }

  def idList = repsep(id, ",")

  def position = (LP ~> coordinate) ~ (COMMA ~> coordinate) ~ (COMMA ~> coordinate) <~ RP ^^ {
    case x ~ y ~ z => Position(x.toDouble, y.toDouble, z.toDouble)
  }

  def areaBoundary = rep1sep(position, COMMA) ^^ { Area }

  def numericId = decimalNumber ^^ { _.toLong }

  def numericIdList: Parser[List[Long]] = rep1sep(numericId, COMMA)

  def gameLocalDate: Parser[LocalDate] = allCharsBeforeNextValue ^^ {
    case timeString => LocalDate.parse(timeString, gameDateFormat)
  }

  def gameLocalTime: Parser[LocalTime] = allCharsBeforeNextValue ^^ {
    case timeString => LocalTime.parse(timeString, gameTimeFormat)
  }

  def uuid = allCharsBeforeNextValue ^^ { UUID.fromString }

  def intKeyValue = (decimalNumber <~ COLUMN) ~ decimalNumber ^^ {
    case key ~ value => (key.toInt, value.toInt)
  }

  def intMap = repsep(intKeyValue, COMMA) ^^ {
    case list => list.toMap
  }

  def eventTime = ("T" ~ COLUMN) ~> decimalNumber ^^ { _.toLong }

  def eventType(x: Int) = ("AType" ~ COLUMN) ~> literal(x.toString) ^^ { _.toLong }

  def eventN[T <: LogEvent](x: Int, p: => Parser[T]) = eventType(x) ~> p

  def gameDate = "GDate" ~ COLUMN ~> gameLocalDate

  def gameTime = "GTime" ~ COLUMN ~> gameLocalTime

  def missionFile = "MFile" ~ COLUMN ~> allCharsBeforeNextValue

  def intValue(key:String) = literal(key) ~ COLUMN ~> decimalNumber ^^ {_.toInt}

  def uuidValue(key:String) = literal(key) ~ COLUMN ~> uuid

  def doubleValue(key:String) = literal(key) ~ COLUMN ~> decimalNumber ^^ {_.toDouble}

  def stringValue(key:String) = literal(key) ~ COLUMN ~> allCharsBeforeNextValue

  def booleanValue(key:String) = literal(key) ~ COLUMN ~> ("0" | "1") ^^ {
    case "0" => false
    case _ => true
  }

  def optIntValue(key:String):Parser[Option[Int]] = literal(key) ~ COLUMN ~> ("-1" | decimalNumber) ^^ {
    case "-1" => None
    case x => Some(x.toInt)
  }

  def missionId = "MID" ~ COLUMN ~> opt(decimalNumber) ^^ { _.map(_.toInt) }
  def counters = ("CNTRS" ~ COLUMN) ~> intMap
  def pos = "POS" ~> position
  def inairValue = "INAIR" ~ COLUMN ~> "\\d".r ^^ { _.toInt }

  // simple values
  def GType = intValue("GType")
  def MODS = intValue("MODS")
  def PRESET = intValue("PRESET")
  def SETTS = stringValue("SETTS")
  def AQMID = intValue("AQMID")
  def AID = intValue("AID")
  def TID = intValue("TID")
  def PLID = intValue("PLID")
  def ID = intValue("ID")
  def PID = intValue("PID")
  def PID_OPT = optIntValue("PID")
  def BUL = intValue("BUL")
  def SH = intValue("SH")
  def BOMB = intValue("BOMB")
  def RCT = intValue("RCT")
  def COUNTRY = intValue("COUNTRY")
  def AMMO = stringValue("AMMO")
  def NAME = stringValue("NAME")
  def TYPE = stringValue("TYPE")
  def FORM = intValue("FORM")
  def FIELD = intValue("FIELD")
  def DMG = doubleValue("DMG")
  def IDS_UUID =  uuidValue("IDS")
  def LOGIN_UUID =  uuidValue("LOGIN")
  def PARENT =  optIntValue("PARENT")
  def PAYLOAD =  intValue("PAYLOAD")
  def FUEL =  doubleValue("FUEL")
  def WM =  intValue("WM")
  def SKIN =  "SKIN" ~ COLUMN ~> opt(allCharsBeforeNextValue)
  def IDS_LIST = "IDS"~ COLUMN ~> idList
  def GID = intValue("GID")
  def LID = intValue("LID")

  def someEvent = eventN(0, missionStart) | eventN(1, hitEvent) | eventN(2, damageEvent) | eventN(3, killEvent) |
    eventN(4, playerAmmoEvent) | eventN(5, takeoffEvent) | eventN(6, landingEvent) | /*7*/missionEndEvent | /* todo 8 */
    eventN(9, airfieldInfoEvent) | eventN(10, playerPlaneSpawnEvent) | eventN(11, groupFormationEvent) |
    eventN(12, spawnEvent)

  def event:Parser[(Long, LogEvent)] = eventTime ~ someEvent ^^ {
    case time ~ e => (time, e)
  }

  def missionEndEvent:Parser[MissionEndEvent] = eventType(7) ^^ {case x => MissionEndEvent()}

  def missionStart:Parser[LogEvent] =
    gameDate ~ gameTime ~ missionFile ~ missionId ~ GType ~ counters ~ SETTS ~ MODS ~ PRESET ~ AQMID ^^ {
      case date ~ time ~ map ~ mid ~ gameType ~ countryCounters ~ settingsFlags ~ mods ~ preset ~ aqmId =>
        MissionStartEvent(date, time, map, mid)
    }

  def hitEvent = AMMO ~ AID ~ TID ^^ {
    case ammo ~ aid ~ tid => HitEvent(ammo, aid, tid)
  }

  def damageEvent = DMG ~ AID ~ TID ~ pos ^^ {
    case damage ~ aid ~ tid ~ pos => DamageEvent(damage, aid, tid, pos)
  }

  def killEvent = AID ~ TID ~ pos ^^ {
    case aid ~ tid ~ pos => KillEvent(aid, tid, pos)
  }

  def takeoffEvent = PID ~ pos ^^ {
    case pid ~ position => TakeoffEvent(pid, position)
  }

  def landingEvent = PID ~ pos ^^ {
    case pid ~ position => LandingEvent(pid, position)
  }

  def airfieldInfoEvent = (AID ~ COUNTRY ~ pos) <~ "IDS()" ^^ {
    case aid ~ country ~ pos => AirFieldInfoEvent(aid, country, pos)
  }

  def planeArmament = BUL ~ SH ~ BOMB ~ RCT ^^ {
    case bul ~ sh ~ bomb ~ rct => PlaneArmament(bul, sh, bomb, rct)
  }

  def playerAmmoEvent = PLID ~ PID ~ planeArmament ~ position ^^ {
    case plid ~ pid ~ planeArmament ~ position => PlayerAmmoEvent(plid, pid, planeArmament, position)
  }

  def playerPlaneSpawnEvent = PLID ~ PID ~ planeArmament ~ position ~ IDS_UUID ~ LOGIN_UUID ~ NAME ~ TYPE ~ COUNTRY ~
    FORM ~ FIELD ~ inairValue ~ PARENT ~ PAYLOAD ~ FUEL ~ SKIN ~ WM ^^ {
    case plId ~ pid ~ arm ~ pos ~ nickUuid ~ loginUuid ~ nickname ~ vType ~ cId ~ form ~ fid ~ inAir ~ parent ~ pl ~ fuel ~ skin ~ wm =>
      PlayerPlaneSpawnEvent(plId, pid, arm, pos, nickUuid, loginUuid, nickname, vType, cId, form, fid, inAir, parent, pl, fuel, skin, wm)
  }

  def groupFormationEvent = GID ~ IDS_LIST ~ LID ^^ {
    case groupId ~ ids ~ leaderId => GroupFormationEvent(groupId, ids, leaderId)
  }

  def spawnEvent = ID ~ TYPE ~ COUNTRY ~ NAME ~ PID_OPT ^^ {
    case id ~ vType ~ countryId ~ name ~ pid => SpawnEvent(id, vType, countryId, name, pid)
  }
}

