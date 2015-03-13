package com.ivanobulo.il2bos.event

import java.time.{LocalDate, LocalTime}
import java.util.UUID

trait LogEvent

case class HitEvent(ammo: String, attackerId: Int, targetId: Int) extends LogEvent

case class DamageEvent(damage: Double, attackerId: Int, targetId: Int, pos: Position) extends LogEvent

case class KillEvent(attackerId: Int, targetId: Int, pos: Position) extends LogEvent

case class PlayerAmmoEvent(playerId: Int, planeId: Int, planeArmament: PlaneArmament, pos: Position) extends LogEvent

case class TakeoffEvent(playerId: Int, pos: Position) extends LogEvent

case class LandingEvent(playerId: Int, pos: Position) extends LogEvent

case class AirFieldInfoEvent(airfieldId: Int, countryId: Int, pos: Position) extends LogEvent

case class MissionEndEvent() extends LogEvent

case class MissionStartEvent(gameDate: LocalDate, gameTime: LocalTime, map: String, missionId: Option[Int]) extends LogEvent

case class PlayerPlaneSpawnEvent(planeId: Int, playerId: Int, armament: PlaneArmament, position: Position, nickId: UUID,
                                 loginId: UUID, playerNickname: String, vehicleType: String, countryId: Int, form: Int,
                                 fieldId: Int, inAir: Int, parent: Option[Int], payload: Int, fuel: Double,
                                 skin: Option[String], weaponMods: Int) extends LogEvent

case class GroupFormationEvent(groupId:Int, ids:List[Int], leaderId:Int) extends LogEvent

case class ObjectIdentificationEvent(id:Int, vehicleType:String, countryId: Int, name:String, playerId:Option[Int]) extends LogEvent

case class PlayerIdentificationEvent(loginId: UUID, nickId: UUID) extends LogEvent

case class PlayerLeaveEvent(loginId: UUID, nickId: UUID) extends LogEvent

case class InfluenceAreaInfoEvent(aid:Int, country:Int, enabled: Boolean, numberOfPlanes:List[Int]) extends LogEvent

case class InfluenceAreaBoundaryEvent(aid:Int, vertices:List[Position]) extends LogEvent

case class VersionEvent(version:Int) extends LogEvent

case class BotSpawnEvent(botId:Int, pos:Position) extends LogEvent

case class PlaneArmament(bullets: Int, shells: Int, bombs: Int, rockets: Int)

case class TimestampedEvent(time: Long, event: LogEvent)

case class Position(x: Double, y: Double, z: Double)

case class Area(positions: List[Position])
