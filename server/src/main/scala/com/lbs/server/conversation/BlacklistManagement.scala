package com.lbs.server.conversation

import akka.actor.ActorSystem
import com.lbs.api.json.model.{DictionaryCity, DictionaryServiceVariants, IdName}
import com.lbs.bot._
import com.lbs.bot.model.{Button, Command}
import com.lbs.server.conversation.BlacklistManagement._
import com.lbs.server.conversation.Login.UserId
import com.lbs.server.conversation.StaticData.StaticDataConfig
import com.lbs.server.conversation.base.Conversation
import com.lbs.server.lang.{Localizable, Localization}
import com.lbs.server.repository.model.DoctorBlacklist
import com.lbs.server.service.{ApiService, DataService}
import com.lbs.server.util.MessageExtractors.{CallbackCommand, IntString, TextCommand}
import com.lbs.server.util.ServerModelConverters._

class BlacklistManagement(
  val userId: UserId,
  bot: Bot,
  apiService: ApiService,
  dataService: DataService,
  val localization: Localization,
  staticDataFactory: UserIdWithOriginatorTo[StaticData]
)(implicit val actorSystem: ActorSystem)
    extends Conversation[BlacklistData]
    with StaticDataForBooking
    with Localizable {

  private[conversation] val staticData = staticDataFactory(userId, self)

  entryPoint(showBlacklist, BlacklistData())

  def showBlacklist: Step =
    ask { _ =>
      val blacklistedDoctors = dataService.getBlacklistedDoctors(userId.userId, userId.accountId)

      if (blacklistedDoctors.isEmpty) {
        bot.sendMessage(
          userId.source,
          lang.blacklistManagementHeader + "\n\n" + lang.noBlacklistedDoctors,
          inlineKeyboard = createInlineKeyboard(
            Seq(Button(lang.addDoctorToBlacklist, Tags.AddDoctor)),
            columns = 1
          )
        )
      } else {
        val entries = blacklistedDoctors.zipWithIndex.map { case (doctor, index) =>
          lang.blacklistEntry(doctor.doctorName, index)
        }.mkString("\n")

        bot.sendMessage(
          userId.source,
          lang.blacklistManagementHeader + "\n\n" + entries,
          inlineKeyboard = createInlineKeyboard(
            Seq(Button(lang.addDoctorToBlacklist, Tags.AddDoctor)),
            columns = 1
          )
        )
      }
    } onReply {
      case Msg(Command(_, _, Some(Tags.AddDoctor)), _) =>
        goto(askCityForAdd)
      case Msg(TextCommand(s"/remove_$IntString(index)"), _) =>
        val blacklistedDoctors = dataService.getBlacklistedDoctors(userId.userId, userId.accountId)
        if (index >= 0 && index < blacklistedDoctors.size) {
          val doctor = blacklistedDoctors(index)
          dataService.removeDoctorFromBlacklist(userId.userId, userId.accountId, doctor.doctorId)
          bot.sendMessage(userId.source, lang.doctorRemovedFromBlacklist(doctor.doctorName))
          goto(showBlacklist)
        } else {
          stay()
        }
    }

  def askCityForAdd: Step =
    staticData(cityConfig) { bd: BlacklistData =>
      withFunctions[DictionaryCity](
        latestOptions = dataService.getLatestCities(userId.accountId),
        staticOptions = apiService.getAllCities(userId.accountId),
        applyId = id => bd.copy(cityId = Some(id.toIdName))
      )
    }(requestNext = askServiceForAdd)

  def askServiceForAdd: Step =
    staticData(serviceConfig) { bd: BlacklistData =>
      withFunctions[DictionaryServiceVariants](
        latestOptions = dataService.getLatestServicesByCityIdAndClinicId(userId.accountId, bd.cityId.get.id, None),
        staticOptions = apiService.getAllServices(userId.accountId),
        applyId = id => bd.copy(serviceId = Some(id.toIdName))
      )
    }(requestNext = askClinicForAdd)

  def askClinicForAdd: Step =
    staticData(clinicConfig) { bd: BlacklistData =>
      withFunctions[IdName](
        latestOptions = dataService.getLatestClinicsByCityId(userId.accountId, bd.cityId.get.id),
        staticOptions = apiService.getAllFacilities(userId.accountId, bd.cityId.get.id, bd.serviceId.get.id),
        applyId = id => bd.copy(clinicId = Some(id))
      )
    }(requestNext = askDoctorForAdd)

  def askDoctorForAdd: Step =
    staticData(doctorConfig) { bd: BlacklistData =>
      withFunctions[IdName](
        latestOptions = dataService.getLatestDoctorsByCityIdAndClinicIdAndServiceId(
          userId.accountId,
          bd.cityId.get.id,
          bd.clinicId.get.optionalId,
          bd.serviceId.get.id
        ),
        staticOptions = apiService
          .getAllDoctors(userId.accountId, bd.cityId.get.id, bd.serviceId.get.id)
          .map(
            _.filter(doc => {
              val clinicId = bd.clinicId.get.optionalId
              clinicId.isEmpty || doc.facilityGroupIds.exists(_.contains(clinicId.get))
            })
            .map(_.toIdName)
          ),
        applyId = id => bd.copy(doctorId = Some(id.toIdName))
      )
    }(requestNext = confirmAddDoctor)

  def confirmAddDoctor: Step =
    ask { bd =>
      val doctorId = bd.doctorId.get.id
      val doctorName = bd.doctorId.get.name

      if (dataService.isDoctorBlacklisted(userId.userId, userId.accountId, doctorId)) {
        bot.sendMessage(userId.source, lang.doctorAlreadyBlacklisted(doctorName))
        goto(showBlacklist)
      } else {
        dataService.addDoctorToBlacklist(userId.userId, userId.accountId, doctorId, doctorName)
        bot.sendMessage(userId.source, lang.doctorAddedToBlacklist(doctorName))
        goto(showBlacklist)
      }
    } onReply {
      case _ => stay() // This step auto-transitions, but we need onReply
    }
}

object BlacklistManagement {

  case class BlacklistData(
    cityId: Option[IdName] = None,
    serviceId: Option[IdName] = None,
    clinicId: Option[IdName] = None,
    doctorId: Option[IdName] = None
  )

  object Tags {
    val AddDoctor = "add_doctor"
    val RemoveDoctor = "remove_doctor"
  }
}
