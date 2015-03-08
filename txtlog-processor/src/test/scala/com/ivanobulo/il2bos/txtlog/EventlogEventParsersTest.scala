package com.ivanobulo.il2bos.txtlog

import java.time.{LocalDate, LocalTime}

import org.specs2.matcher.ParserMatchers
import org.specs2.mutable._

class EventlogEventParsersTest extends Specification with ParserMatchers {
  override val parsers = new EventlogEventParsers()

  "intMap" should {
    "recognize maps" in {
      parsers.intMap must succeedOn("2:1,3:2").withResult(Map((2, 1), (3, 2)))
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
      parsers.event must succeedOn(eventString)
        .withResult((31598L, PlayerAmmoEvent(311297, 394241, 1158, 10, 1, 0, Position(124204.711, 131.871, 240163.422))))
    }
  }
}
