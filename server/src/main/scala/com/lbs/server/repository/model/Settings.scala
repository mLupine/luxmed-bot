package com.lbs.server.repository.model

import javax.persistence.{Access, AccessType, Column, Entity}
import scala.beans.BeanProperty

@Entity
@Access(AccessType.FIELD)
class Settings extends RecordId {
  @BeanProperty
  @Column(name = "user_id", unique = true, nullable = false)
  var userId: JLong = _

  @BeanProperty
  @Column(nullable = false)
  var lang: Int = 0 // En by default

  @BeanProperty
  @Column(nullable = false)
  var defaultOffset: Int = 0

  @BeanProperty
  @Column(nullable = false)
  var alwaysAskOffset: Boolean = false

  @BeanProperty
  @Column(name = "filter_blacklisted_doctors_in_manual_search", nullable = false)
  var filterBlacklistedDoctorsInManualSearch: Boolean = false
}

object Settings {
  def apply(userId: Long, lang: Int, defaultOffset: Int, alwaysAskOffset: Boolean, filterBlacklistedDoctorsInManualSearch: Boolean = false): Settings = {
    val settings = new Settings
    settings.userId = userId
    settings.lang = lang
    settings.defaultOffset = defaultOffset
    settings.alwaysAskOffset = alwaysAskOffset
    settings.filterBlacklistedDoctorsInManualSearch = filterBlacklistedDoctorsInManualSearch
    settings
  }
}
