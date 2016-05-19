import java.time.LocalDateTime

import GarageSpot.SpotId

/**
  * Created by Vincent on 5/15/16.
  */
object GarageSpot {
  type SpotId = Int
}

class GarageSpot[T <: GarageVehicle](val id: SpotId) {

  var vehicle: Option[T] = None

  def isOccupied: Boolean = {
    vehicle match {

      case Some(v) => (v.enterDateTime, v.leaveDateTime) match {
        case (Some(_), Some(d)) =>
          println("spot " + id + " occupied by " + v)
          d.isAfter(LocalDateTime.now)
        case (None, None) =>
          println("spot " + id + " not occupied by " + v)
          false
        case _ =>
          println("spot " + id + " occupied")
          true
      }
      case None =>
        println("spot " + id + " not occupied")
        false
    }
  }
}

