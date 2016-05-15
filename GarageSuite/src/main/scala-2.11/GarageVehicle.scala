import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import GarageSpot.SpotId

/**
  * Created by Vincent on 5/15/16.
  */
object GarageVehicle {

  object Trunk {
    val hourlyRate: Double = 20
  }

  object Car {
    val hourlyRate: Double = 15
  }

}

sealed trait GarageVehicle {
  var spot: Option[SpotId] = None
  var enterDateTime: Option[LocalDateTime] = None
  var leaveDateTime: Option[LocalDateTime] = None

  def getHourlyRate: Double

  def getFee: Double = (enterDateTime, leaveDateTime) match {
    case (Some(e), Some(l)) => ChronoUnit.HOURS.between(e, l) * getHourlyRate
    case _ => 0.0
  }

  val getLicenseNo: String
}


case class Truck(licenseNo: String) extends GarageVehicle {
  def getHourlyRate: Double = GarageVehicle.Trunk.hourlyRate

  val getLicenseNo: String = licenseNo
}

case class Car(licenseNo: String) extends GarageVehicle {
  def getHourlyRate: Double = GarageVehicle.Car.hourlyRate

  val getLicenseNo: String = licenseNo
}
