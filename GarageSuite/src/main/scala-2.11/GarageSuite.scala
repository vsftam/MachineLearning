import java.time.LocalDateTime

/**
  * Created by Vincent on 5/8/16.
  */

class Garage(carGarageSpots: Array[GarageSpot[Car]], truckGarageSpots: Array[GarageSpot[Truck]]) {
  def park[V <: GarageVehicle](vehicle: V, parkTime: Option[LocalDateTime]): Boolean = {

    def parkHelper[V <: GarageVehicle](vehicle: V, garageSpot: Option[GarageSpot[V]]): Boolean = {
      garageSpot match {
        case Some(s) =>
          vehicle.enterDateTime = Some(parkTime.getOrElse(LocalDateTime.now))
          vehicle.spot = Some(s.spotId)
          s.vehicle = Some(vehicle)
          println("Park car " + vehicle.getLicenseNo + " at spot " + s.spotId)
          true
        case None => false
      }
    }

    vehicle match {
      case c: Car =>
        val spot: Option[GarageSpot[Car]] = carGarageSpots.find(spot => !spot.isOccupied)
        parkHelper(c, spot)
      case t: Truck =>
        val spot: Option[GarageSpot[Truck]] = truckGarageSpots.find(spot => !spot.isOccupied)
        parkHelper(t, spot)
    }
  }

  def unpark[V <: GarageVehicle](vehicle: V, unparkTime: Option[LocalDateTime]): Boolean = {

    def unparkHelper[V <: GarageVehicle](vehicle: V, garageSpot: Option[GarageSpot[V]]): Boolean = {
      garageSpot match {
        case Some(s) =>
          vehicle.leaveDateTime = Some(unparkTime.getOrElse(LocalDateTime.now))
          vehicle.spot = None
          s.vehicle = None
          println("Unpark vehicle " + vehicle.getLicenseNo + " from spot " + s.spotId + " with fee " + vehicle.getFee)
          true
        case None => false
      }
    }

    vehicle match {
      case c: Car => c.spot match {
        case Some(carSpot) =>
          val spot: Option[GarageSpot[Car]] = carGarageSpots.find(s => s.spotId == carSpot)
          unparkHelper(c, spot)
        case None => false
      }
      case t: Truck => t.spot match {
        case Some(truckSpot) =>
          val spot: Option[GarageSpot[Truck]] = truckGarageSpots.find(s => s.spotId == truckSpot)
          unparkHelper(t, spot)
        case None => false
      }
    }
  }
}

object GarageSuite extends App {

  var carSpots = Array[GarageSpot[Car]]()
  carSpots = carSpots :+ new GarageSpot[Car](1)
  carSpots = carSpots :+ new GarageSpot[Car](2)
  carSpots = carSpots :+ new GarageSpot[Car](3)
  var truckSpots = Array[GarageSpot[Truck]]()
  truckSpots = truckSpots :+ new GarageSpot[Truck](4)
  truckSpots = truckSpots :+ new GarageSpot[Truck](5)

  val myGarage = new Garage(carSpots, truckSpots)

  val now = LocalDateTime.now

  val car1 = new Car("C123")
  val c = myGarage.park(car1, Some(now))
  if (!c) println("cannot park " + car1)
  val truck1 = new Truck("T234")
  val t = myGarage.park(truck1, Some(now))

  val twoHrsLater = now.plusHours(2)

  if (!t)
    println("cannot park" + truck1)
  else
    myGarage.unpark(truck1, Some(twoHrsLater))

  if (c)
    myGarage.unpark(car1, Some(twoHrsLater))
}