package com.lbs.server.service

import com.lbs.api.json.model.*
import com.lbs.bot.Bot
import com.lbs.bot.model.MessageSource
import com.lbs.server.lang.{En, Localization}
import com.lbs.server.repository.model.Monitoring
import com.lbs.server.util.DateTimeUtil.minutesSinceBeginOf2018
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.*
import org.mockito.Mockito.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar

import java.time.{LocalDateTime, LocalTime, ZonedDateTime}

class MonitoringServiceSpec extends AnyWordSpec with MockitoSugar {

  private val blockedId = 50L
  private val allowedId = 77L

  private def doctor(id: Long, first: String, last: String) =
    Doctor(Some("dr"), Some(List(10L)), Some(first), Some(false), Some(1L), id, Some(last))

  private def term(id: Long, first: String, last: String, scheduleId: Long): TermExt = {
    val dt = LuxmedFunnyDateTime(dateTimeLocal = Some(LocalDateTime.of(2026, 6, 1, 10, 0)))
    val visit = Term(
      Some("Clinic A"),
      10L,
      5L,
      dt,
      dt,
      doctor(id, first, last),
      Some(""),
      isAdditional = false,
      isImpediment = false,
      isTelemedicine = false,
      1L,
      scheduleId,
      100L
    )
    TermExt(AdditionalData(isPreparationRequired = false, preparationItems = Nil), visit)
  }

  private val blocked = term(blockedId, "TARAS", "BLOCKED", 500L)
  private val allowed = term(allowedId, "ANNA", "ALLOWED", 770L)

  private class StoredBlacklist extends DataService {
    var monitoring: Option[Monitoring] = None

    override def getBlacklistedDoctorIds(userId: Long, accountId: Long): Set[Long] =
      if (userId == 1L && accountId == 2L) Set(blockedId) else Set.empty

    override def findMonitoring(accountId: Long, monitoringId: Long): Option[Monitoring] = monitoring

    override def saveMonitoring(monitoring: Monitoring): Monitoring = monitoring
  }

  private def monitoring(autobook: Boolean): Monitoring =
    Monitoring(
      userId = 1L,
      username = "testuser",
      accountId = 2L,
      chatId = "1",
      sourceSystemId = 1L,
      payerId = 1L,
      cityId = 1L,
      cityName = "Wroclaw",
      clinicId = None,
      clinicName = "Any",
      serviceId = 100L,
      serviceName = "GP",
      doctorId = None,
      doctorName = "Any",
      dateFrom = ZonedDateTime.now().minusDays(1),
      dateTo = ZonedDateTime.now().plusDays(7),
      autobook = autobook,
      timeFrom = LocalTime.of(7, 0),
      timeTo = LocalTime.of(21, 0),
      offset = 0
    )

  private def wire(dataService: DataService, apiService: ApiService, bot: Bot): MonitoringService = {
    val localization = mock[Localization]
    when(localization.lang(any())).thenReturn(En)
    val monitoringService = new MonitoringService
    set(monitoringService, "bot", bot)
    set(monitoringService, "dataService", dataService)
    set(monitoringService, "apiService", apiService)
    set(monitoringService, "localization", localization)
    when(
      apiService.getAvailableTerms(
        anyLong(),
        anyLong(),
        any(),
        anyLong(),
        any(),
        any(),
        any(),
        any(),
        any(),
        anyLong()
      )
    ).thenReturn(Right(List(blocked, allowed)))
    monitoringService
  }

  private def set(target: AnyRef, name: String, value: Any): Unit = {
    val field = target.getClass.getDeclaredField(name)
    field.setAccessible(true)
    field.set(target, value)
  }

  private def runMonitor(monitoringService: MonitoringService, monitoring: Monitoring): Unit = {
    val method = monitoringService.getClass.getDeclaredMethod("monitor", classOf[Monitoring])
    method.setAccessible(true)
    method.invoke(monitoringService, monitoring)
  }

  private def stubBooking(apiService: ApiService): Unit = {
    val details = doctor(allowedId, "ANNA", "ALLOWED")
    val valuation = Valuation(
      None,
      Some(1L),
      isExternalReferralAllowed = false,
      isReferralRequired = false,
      Some(1L),
      Some(0.0),
      Some(1L),
      Some(1L),
      Some(1L),
      requireReferralForPP = false,
      1L
    )
    when(apiService.getXsrfToken(anyLong())).thenReturn(Right(XsrfToken("tok", Seq())))
    when(apiService.reservationLockterm(anyLong(), any(), any())).thenReturn(
      Right(
        ReservationLocktermResponse(
          Nil,
          Nil,
          hasErrors = false,
          hasWarnings = false,
          ReservationLocktermResponseValue(
            changeTermAvailable = false,
            None,
            details,
            Nil,
            999L,
            List(valuation)
          )
        )
      )
    )
    when(apiService.reservationConfirm(anyLong(), any(), any())).thenReturn(
      Right(
        ReservationConfirmResponse(
          Nil,
          Nil,
          hasErrors = false,
          hasWarnings = false,
          ReservationConfirmValue(canSelfConfirm = false, "nps", 123L, 999L)
        )
      )
    )
  }

  "monitoring a visit search" must {
    "omit a blacklisted doctor that the API returned" in {
      val api = mock[ApiService]
      val bot = mock[Bot]
      val svc = wire(new StoredBlacklist, api, bot)
      runMonitor(svc, monitoring(autobook = false))

      val captor = ArgumentCaptor.forClass(classOf[String])
      verify(bot).sendMessage(any[MessageSource](), captor.capture())
      assert(captor.getValue.contains("Doctor: ANNA ALLOWED"))
      assert(!captor.getValue.contains("TARAS BLOCKED"))
    }

    "book the other doctor when autobook is on" in {
      val api = mock[ApiService]
      val bot = mock[Bot]
      stubBooking(api)
      val svc = wire(new StoredBlacklist, api, bot)
      runMonitor(svc, monitoring(autobook = true))

      val captor = ArgumentCaptor.forClass(classOf[ReservationLocktermRequest])
      verify(api).reservationLockterm(anyLong(), any(), captor.capture())
      assert(captor.getValue.doctorId == 77L)
      assert(captor.getValue.doctor.name == "ANNA ALLOWED")
    }

    "refuse to book a blacklisted schedule from a monitoring link" in {
      val api = mock[ApiService]
      val bot = mock[Bot]
      stubBooking(api)
      val data = new StoredBlacklist
      val visit = monitoring(autobook = false)
      visit.recordId = 9L
      data.monitoring = Some(visit)
      val svc = wire(data, api, bot)
      svc.bookAppointmentByScheduleId(
        2L,
        9L,
        500L,
        minutesSinceBeginOf2018(blocked.term.dateTimeFrom.get)
      )
      verify(api, never()).reservationLockterm(anyLong(), any(), any())
    }
  }
}
