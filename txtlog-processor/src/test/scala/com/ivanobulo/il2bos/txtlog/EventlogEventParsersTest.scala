package com.ivanobulo.il2bos.txtlog

import java.time.{LocalDate, LocalTime}

import org.specs2.matcher.ParserMatchers
import org.specs2.mutable._

/**
 * Created by Ivan on 3/7/2015.
 */
class EventlogEventParsersTest extends Specification with ParserMatchers {
  override val parsers = new EventlogEventParsers()

  "intMap" should {
    "recognize maps" in {
      parsers.intMap must succeedOn("2:1,3:2").withResult(Map((2,1), (3,2)))
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

  "missionStart" should {
    "parse valid event line" in {
      val sourceLine =
        """T:122 AType:0 GDate:1942.12.24 GTime:14:55:3 MFile:Missions\_gen.msnbin MID: GType:702
          | CNTRS:50:0,101:1,201:2 SETTS:0100000000100001000000001 MODS:0 PRESET:2 AQMID:0""".stripMargin
      val expectedEvent = MissionStartEvent(
        LocalDate.of(1942, 12, 24), LocalTime.of(14, 55, 3), "Missions\\_gen.msnbin", None)

      parsers.missionStart must succeedOn(sourceLine).withResult((122L, expectedEvent))
    }
  }
}
