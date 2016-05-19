import java.time.LocalDateTime

/**
  * Created by Vincent on 5/8/16.
  */

class Garage(carGarageSpots: Array[GarageSpot[Car]], truckGarageSpots: Array[GarageSpot[Truck]]) {
  var feesReceived: Double = 0.0

  def park[V <: GarageVehicle](vehicle: V, parkTime: Option[LocalDateTime]): Boolean = {

    def parkHelper[V <: GarageVehicle](vehicle: V, garageSpot: Option[GarageSpot[V]]): Boolean = {
      garageSpot match {
        case Some(s) =>
          vehicle.enterDateTime = Some(parkTime.getOrElse(LocalDateTime.now))
          vehicle.spot = Some(s.id)
          s.vehicle = Some(vehicle)
          println("Park " + vehicle + " at spot " + s.id)
          true
        case None =>
          println("Garage is full, cannot park " + vehicle)
          false
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
          println("Unpark " + vehicle + " from spot " + s.id + " with fee " + vehicle.getFee)
          feesReceived = feesReceived + vehicle.getFee
          true
        case None => false
      }
    }

    vehicle match {
      case c: Car => c.spot match {
        case Some(carSpot) =>
          val spot: Option[GarageSpot[Car]] = carGarageSpots.find(s => s.id == carSpot)
          unparkHelper(c, spot)
        case None => false
      }
      case t: Truck => t.spot match {
        case Some(truckSpot) =>
          val spot: Option[GarageSpot[Truck]] = truckGarageSpots.find(s => s.id == truckSpot)
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

  val car1 = Car("C123")
  var c = myGarage.park(car1, Some(now))

  val truck1 = Truck("T234")
  var t = myGarage.park(truck1, Some(now))

  val halfHourLater = now.plusMinutes(30)
  val car2 = Car("C278")
  c = myGarage.park(car2, Some(halfHourLater))

  val hourHalfLater = now.plusMinutes(90)
  var car3 = Car("C345")
  c = myGarage.park(car3, Some(hourHalfLater))

  var car4 = Car("C498")
  c = myGarage.park(car4, Some(hourHalfLater))

  val twoHrsLater = now.plusHours(2)

  myGarage.unpark(truck1, Some(twoHrsLater))
  myGarage.unpark(car1, Some(twoHrsLater))
  myGarage.unpark(car2, Some(twoHrsLater))

  myGarage.park(car4, Some(twoHrsLater))

  println("Total income for garage: " + myGarage.feesReceived)
}