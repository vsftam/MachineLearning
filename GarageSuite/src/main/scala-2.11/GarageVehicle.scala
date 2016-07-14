import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import GarageSpot.SpotId

/**
  * Created by Vincent on 5/15/16.
  */
object GarageVehicle {

  object Truck {
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

  def getFee: Double = (enterDateTime, leaveDateTime) match {
    case (Some(e), Some(l)) => ChronoUnit.MINUTES.between(e, l) * getHourlyRate / 60.0
    case _ => 0.0
  }

  def getHourlyRate: Double = this match {
    case t: Truck => GarageVehicle.Truck.hourlyRate
    case c: Car => GarageVehicle.Car.hourlyRate
  }

  override val toString: String = this match {
    case Truck(licenseNo) => "Truck (" + licenseNo + ")"
    case Car(licenseNo) => "Car (" + licenseNo + ")"
  }
}

case class Truck(licenseNo: String) extends GarageVehicle

case class Car(licenseNo: String) extends GarageVehicle
