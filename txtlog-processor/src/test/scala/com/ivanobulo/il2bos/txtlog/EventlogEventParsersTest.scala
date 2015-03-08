package com.ivanobulo.il2bos.txtlog

import java.time.{LocalDate, LocalTime}
import java.util.UUID

import com.ivanobulo.il2bos.event._
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable._

class EventlogEventParsersTest extends Specification with ParserMatchers {
  override val parsers = new EventlogEventParsers()

  implicit def stringToUUID(s:String): UUID = UUID.fromString(s)

  "intMap" should {
    "recognize maps" in {
      parsers.intMap must succeedOn("2:1,3:2").withResult(Map((2, 1), (3, 2)))
    }
  }

  "booleanValue" should {
    "translate boolean values properly" in {
      parsers.booleanValue("BAR") must succeedOn("BAR:0").withResult(false)
      parsers.booleanValue("FOO") must succeedOn("FOO:1").withResult(true)

      parsers.booleanValue("FOO") must failOn("BAR:0")
      parsers.booleanValue("TEST") must failOn("TEST:3")
    }
  }

  "listOfStrings" should {
    "recognize empty lists" in {
      parsers.listOfStrings must succeedOn("()").withResult(List[String]())
    }
    "recognize one element lists" in {
      parsers.listOfStrings must succeedOn("(abc)").withResult(List("abc"))
    }
    "recognize more than one element lists" in {
      parsers.listOfStrings must succeedOn("(abc,cde)").withResult(List("abc", "cde"))
    }
  }

  "eventTime" should {
    "recognize 'T:123'" in {
      parsers.eventTime must succeedOn("T:123").withResult(123L)
    }
  }

  "eventType" should {
    "match correct" in {
      parsers.eventType(7) must succeedOn("AType:7").withResult(7)
      parsers.eventType(0) must failOn("AType:7")
    }
  }

  "PARENT" should {
    "match numeric id" in {
      parsers.PARENT must succeedOn("PARENT:321").withResult(Some(321))
    }
    "handle -1" in {
      parsers.PARENT must succeedOn("PARENT:-1").withResult(None)
    }
  }

  "event parser" should {
    "recognize mission start event" in {
      val sourceLine =
        """T:122 AType:0 GDate:1942.12.24 GTime:14:55:3 MFile:Missions\_gen.msnbin MID: GType:702
          | CNTRS:50:0,101:1,201:2 SETTS:0100000000100001000000001 MODS:0 PRESET:2 AQMID:0""".stripMargin
      val expectedEvent = MissionStartEvent(
        LocalDate.of(1942, 12, 24), LocalTime.of(14, 55, 3), "Missions\\_gen.msnbin", None)

      parsers.event must succeedOn(sourceLine).withResult((122L, expectedEvent))
    }
    "recognize hit event" in {
      val eventString = "T:26455 AType:1 AMMO:BULLET_RUS_7-62x54_AP AID:311297 TID:454656"
      parsers.event must succeedOn(eventString)
        .withResult((26455L, HitEvent("BULLET_RUS_7-62x54_AP", 311297, 454656)))
    }
    "recognize damage event" in {
      val eventString = "T:26458 AType:2 DMG:0.030 AID:311297 TID:454656 POS(123603.250,145.485,242323.359)"
      parsers.event must succeedOn(eventString)
        .withResult((26458L, DamageEvent(0.03, 311297, 454656, Position(123603.250, 145.485, 242323.359))))
    }
    "recognize player ammo event" in {
      val eventString = "T:31598 AType:4 PLID:311297 PID:394241 BUL:1158 SH:10 BOMB:1 RCT:0 (124204.711,131.871,240163.422)"

      val expectedEvent = PlayerAmmoEvent(311297, 394241, PlaneArmament(1158, 10, 1, 0),
        Position(124204.711, 131.871, 240163.422))

      parsers.event must succeedOn(eventString).withResult((31598L, expectedEvent))
    }
    "recognize takeoff event" in {
      val eventString = "T:31598 AType:5 PID:123 POS(124204.711,131.871,240163.422)"
      parsers.event must succeedOn(eventString)
        .withResult((31598L, TakeoffEvent(123, Position(124204.711, 131.871, 240163.422))))
    }
    "recognize landing event" in {
      val eventString = "T:31598 AType:6 PID:123 POS(124204.711,131.871,240163.422)"
      parsers.event must succeedOn(eventString)
        .withResult((31598L, LandingEvent(123, Position(124204.711, 131.871, 240163.422))))
    }
    "recognize mission end event" in {
      val eventString = "T:31598 AType:7"
      parsers.event must succeedOn(eventString)
        .withResult((31598L, MissionEndEvent()))
    }
    "recognize airfield info event" in {
      val eventString = "T:10 AType:9 AID:96256 COUNTRY:201 POS(133798.813, 82.420, 185350.141) IDS()"
      parsers.event must succeedOn(eventString)
        .withResult((10L, AirFieldInfoEvent(96256, 201, Position(133798.813, 82.420, 185350.141))))
    }
    "recognize player plane spawn event" in {
      val eventString =
        """T:33969 AType:10 PLID:697346 PID:698370 BUL:1620 SH:0 BOMB:0 RCT:0
          | (119624.836,56.748,157956.859) IDS:162c3112-4818-42e4-849e-7053d294a63d
          | LOGIN:16c41bea-6d4f-477e-afba-2556ed240772 NAME:50BWHerrKlink TYPE:Yak-1 ser.69 COUNTRY:101 FORM:0
          | FIELD:876544 INAIR:2 PARENT:-1 PAYLOAD:0 FUEL:1.000 SKIN: WM:1""".stripMargin

      val expectedEvent = PlayerPlaneSpawnEvent(697346, 698370, PlaneArmament(1620, 0, 0, 0),
        Position(119624.836, 56.748, 157956.859), "162c3112-4818-42e4-849e-7053d294a63d",
        "16c41bea-6d4f-477e-afba-2556ed240772", "50BWHerrKlink", "Yak-1 ser.69", 101, 0, 876544, 2, None, 0, 1.0, None, 1)

      parsers.event must succeedOn(eventString).withResult((33969L, expectedEvent))
    }
    "recognize player plane spawn event with optional present" in {
      val eventString =
        """T:33969 AType:10 PLID:697346 PID:698370 BUL:1620 SH:0 BOMB:0 RCT:0
          | (119624.836,56.748,157956.859) IDS:162c3112-4818-42e4-849e-7053d294a63d
          | LOGIN:16c41bea-6d4f-477e-afba-2556ed240772 NAME:50BWHerrKlink TYPE:Yak-1 ser.69 COUNTRY:101 FORM:0
          | FIELD:876544 INAIR:2 PARENT:123 PAYLOAD:0 FUEL:1.000 SKIN:funkySkin.png WM:1""".stripMargin

      val expectedEvent = PlayerPlaneSpawnEvent(697346, 698370, PlaneArmament(1620, 0, 0, 0),
        Position(119624.836, 56.748, 157956.859), "162c3112-4818-42e4-849e-7053d294a63d",
        "16c41bea-6d4f-477e-afba-2556ed240772", "50BWHerrKlink", "Yak-1 ser.69", 101, 0, 876544, 2, Some(123), 0, 1.0,
        Some("funkySkin.png"), 1)

      parsers.event must succeedOn(eventString).withResult((33969L, expectedEvent))
    }
  }
}
