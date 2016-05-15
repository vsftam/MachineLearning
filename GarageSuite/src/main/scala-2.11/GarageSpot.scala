import java.time.LocalDateTime

import GarageSpot.SpotId

/**
  * Created by Vincent on 5/15/16.
  */
object GarageSpot {
  type SpotId = Int
}

class GarageSpot[T <: GarageVehicle](id: SpotId) {

  var vehicle: Option[T] = None

  def spotId = id

  def isOccupied: Boolean = {
    vehicle match {

      case Some(v) => (v.enterDateTime, v.leaveDateTime) match {
        case (Some(_), Some(d)) =>
          println("spot " + spotId + " occupied by " + v.getLicenseNo)
          d.isAfter(LocalDateTime.now)
        case (None, None) =>
          println("spot " + spotId + " not occupied by " + v.getLicenseNo)
          false
        case _ =>
          println("spot " + spotId + " occupied")
          true
      }
      case None =>
        println("spot " + spotId + " not occupied")
        false
    }
  }
}

