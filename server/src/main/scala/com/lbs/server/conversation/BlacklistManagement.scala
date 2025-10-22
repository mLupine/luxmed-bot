package com.lbs.server.conversation

import akka.actor.ActorSystem
import com.lbs.api.json.model.{DictionaryCity, DictionaryServiceVariants, IdName}
import com.lbs.bot._
import com.lbs.bot.model.{Button, Command}
import com.lbs.server.conversation.BlacklistManagement._
import com.lbs.server.conversation.Login.UserId
import com.lbs.server.conversation.StaticData.{FindOptions, FoundOptions, LatestOptions, StaticDataConfig}
import com.lbs.server.conversation.base.Conversation
import com.lbs.server.lang.{Localizable, Localization}
import com.lbs.server.repository.model.DoctorBlacklist
import com.lbs.server.service.{ApiService, DataService}
import com.lbs.server.ThrowableOr
import com.lbs.server.util.MessageExtractors.TextCommand
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
      case Msg(TextCommand(removePattern(index)), _) =>
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
    ask { _ =>
      staticData.restart()
      staticData ! cityConfig
    } onReply {
      case Msg(cmd: Command, _) =>
        staticData ! cmd
        stay()
      case Msg(LatestOptions, _) =>
        staticData ! LatestOptions(dataService.getLatestCities(userId.accountId))
        stay()
      case Msg(FindOptions(searchText), _) =>
        staticData ! FoundOptions(filterOptions(apiService.getAllCities(userId.accountId), searchText))
        stay()
      case Msg(id: IdName, bd: BlacklistData) =>
        goto(askServiceForAdd) using bd.copy(cityId = Some(id))
    }

  def askServiceForAdd: Step =
    ask { _ =>
      staticData.restart()
      staticData ! serviceConfig
    } onReply {
      case Msg(cmd: Command, _) =>
        staticData ! cmd
        stay()
      case Msg(LatestOptions, bd: BlacklistData) =>
        staticData ! LatestOptions(
          dataService.getLatestServicesByCityIdAndClinicId(userId.accountId, bd.cityId.get.id, None)
        )
        stay()
      case Msg(FindOptions(searchText), _) =>
        staticData ! FoundOptions(filterOptions(apiService.getAllServices(userId.accountId), searchText))
        stay()
      case Msg(id: IdName, bd: BlacklistData) =>
        goto(askClinicForAdd) using bd.copy(serviceId = Some(id))
    }

  def askClinicForAdd: Step =
    ask { _ =>
      staticData.restart()
      staticData ! clinicConfig
    } onReply {
      case Msg(cmd: Command, _) =>
        staticData ! cmd
        stay()
      case Msg(LatestOptions, bd: BlacklistData) =>
        staticData ! LatestOptions(dataService.getLatestClinicsByCityId(userId.accountId, bd.cityId.get.id))
        stay()
      case Msg(FindOptions(searchText), bd: BlacklistData) =>
        staticData ! FoundOptions(
          filterOptions(apiService.getAllFacilities(userId.accountId, bd.cityId.get.id, bd.serviceId.get.id), searchText)
        )
        stay()
      case Msg(id: IdName, bd: BlacklistData) =>
        goto(askDoctorForAdd) using bd.copy(clinicId = Some(id))
    }

  def askDoctorForAdd: Step =
    ask { _ =>
      staticData.restart()
      staticData ! doctorConfig
    } onReply {
      case Msg(cmd: Command, _) =>
        staticData ! cmd
        stay()
      case Msg(LatestOptions, bd: BlacklistData) =>
        staticData ! LatestOptions(
          dataService.getLatestDoctorsByCityIdAndClinicIdAndServiceId(
            userId.accountId,
            bd.cityId.get.id,
            bd.clinicId.get.optionalId,
            bd.serviceId.get.id
          )
        )
        stay()
      case Msg(FindOptions(searchText), bd: BlacklistData) =>
        val doctors = apiService
          .getAllDoctors(userId.accountId, bd.cityId.get.id, bd.serviceId.get.id)
          .map(
            _.filter(doc => {
              val clinicId = bd.clinicId.get.optionalId
              clinicId.isEmpty || doc.facilityGroupIds.exists(_.contains(clinicId.get))
            })
            .map(_.toIdName)
          )
        staticData ! FoundOptions(filterOptions(doctors, searchText))
        stay()
      case Msg(id: IdName, bd: BlacklistData) =>
        goto(confirmAddDoctor) using bd.copy(doctorId = Some(id))
    }

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

  private def cityConfig = StaticDataConfig(lang.city, "wro", "Wrocław", isAnyAllowed = false)
  private def clinicConfig = StaticDataConfig(lang.clinic, "swob", "Swobodna 1", isAnyAllowed = true)
  private def serviceConfig = StaticDataConfig(lang.service, "stomat", "Stomatolog", isAnyAllowed = false)
  private def doctorConfig = StaticDataConfig(lang.doctor, "Kowal", "Kowalski", isAnyAllowed = false)

  private def filterOptions[T <: com.lbs.api.json.model.Identified](
    options: ThrowableOr[List[T]],
    searchText: String
  ) = {
    options.map(opt => opt.filter(c => c.name.toLowerCase.contains(searchText)))
  }

  private val removePattern = "/remove_(\\d+)".r
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
