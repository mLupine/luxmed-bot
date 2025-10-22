package com.lbs.server.repository.model

import javax.persistence.{Access, AccessType, Column, Entity}
import scala.beans.BeanProperty

@Entity
@Access(AccessType.FIELD)
class DoctorBlacklist extends RecordId {
  @BeanProperty
  @Column(name = "user_id", nullable = false)
  var userId: JLong = _

  @BeanProperty
  @Column(name = "account_id", nullable = false)
  var accountId: JLong = _

  @BeanProperty
  @Column(name = "doctor_id", nullable = false)
  var doctorId: JLong = _

  @BeanProperty
  @Column(name = "doctor_name", nullable = false)
  var doctorName: String = _
}

object DoctorBlacklist {
  def apply(userId: Long, accountId: Long, doctorId: Long, doctorName: String): DoctorBlacklist = {
    val blacklist = new DoctorBlacklist
    blacklist.userId = userId
    blacklist.accountId = accountId
    blacklist.doctorId = doctorId
    blacklist.doctorName = doctorName
    blacklist
  }
}
