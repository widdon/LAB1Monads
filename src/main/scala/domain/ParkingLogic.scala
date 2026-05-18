package domain
import monads._

object ParkingLogic {

  import ParkingService._
  import ParkingConfigService._

  private def findFree(occupied: Set[Int], capacity: Int): Option[Int] =
    (1 to capacity).find(spot => !occupied.contains(spot))

  // Были выведены
  private def enterError(message: String): Writer[Log, Either[String, Int]] =
    for {
      _ <- logError(message)
    } yield Left(message): Either[String, Int]

  private def exitError(message: String): Writer[Log, Either[String, Double]] =
    for {
      _ <- logError(message)
    } yield Left(message): Either[String, Double]

  private def successfulEnter(
                               carNumber: String,
                               state: ParkingState,
                               spot: Int
                             ): (ParkingState, Writer[Log, Either[String, Int]]) = {

    val updatedState = state.copy(
      occupiedSpots = state.occupiedSpots + spot,
      entryHours = state.entryHours + (carNumber -> state.currentHour),
      spotByCar = state.spotByCar + (carNumber -> spot)
    )

    val result =
      for {
        _ <- logEnter(carNumber)
        _ <- logAssignedSpot(carNumber, spot)
      } yield Right(spot): Either[String, Int]

    (updatedState, result)
  }

  private def successfulExit(
                              carNumber: String,
                              state: ParkingState,
                              cost: Double
                            ): (ParkingState, Writer[Log, Either[String, Double]]) = {

    val spot = state.spotByCar(carNumber)

    val updatedState = state.copy(
      occupiedSpots = state.occupiedSpots - spot,
      entryHours = state.entryHours - carNumber,
      spotByCar = state.spotByCar - carNumber,
      profit = state.profit + cost
    )

    val result =
      for {
        _ <- logRate(cost)
        _ <- logExit(carNumber)
      } yield Right(cost): Either[String, Double]

    (updatedState, result)
  }

  private def successfulLostTicket(
                                    carNumber: String,
                                    state: ParkingState,
                                    fine: Double
                                  ): (ParkingState, Writer[Log, Either[String, Double]]) = {

    val spot = state.spotByCar(carNumber)

    val updatedState = state.copy(
      occupiedSpots = state.occupiedSpots - spot,
      entryHours = state.entryHours - carNumber,
      spotByCar = state.spotByCar - carNumber,
      profit = state.profit + fine
    )

    val result =
      for {
        _ <- logLostTicket(carNumber, fine)
      } yield Right(fine): Either[String, Double]

    (updatedState, result)
  }

  def enterCar(
                carNumber: String
              ): Reader[
    ParkingConfig,
    State[ParkingState, Writer[Log, Either[String, Int]]]
  ] =
    Reader { config =>
      State { state =>

        val freePlaces = config.capacity - state.occupiedSpots.size

        if (state.entryHours.contains(carNumber)) {
          (state, enterError(s"Машина $carNumber уже находится на парковке"))

        } else if (freePlaces <= 0) {
          (state, enterError("Нет свободных мест"))

        } else {
          findFree(state.occupiedSpots, config.capacity) match {
            case Some(spot) =>
              successfulEnter(carNumber, state, spot)

            case None =>
              (state, enterError("Свободное место не найдено"))
          }
        }
      }
    }

  def exitCar(
               carNumber: String
             ): Reader[
    ParkingConfig,
    State[ParkingState, Writer[Log, Either[String, Double]]]
  ] =
    Reader { config =>
      State { state =>

        state.entryHours.get(carNumber) match {

          case None =>
            (state, exitError(s"Машина $carNumber не найдена"))

          case Some(entryTime) =>
            val exitTime = state.currentHour
            val cost = totalCost(entryTime, exitTime).run(config)

            successfulExit(carNumber, state, cost)
        }
      }
    }

  def nextHour: State[ParkingState, Writer[Log, Int]] =
    State { state =>

      val newHour = state.currentHour + 1

      val updatedState = state.copy(
        currentHour = newHour
      )

      val result =
        Writer(
          Vector(s"Время переведено. Текущий час: $newHour"),
          newHour
        )

      (updatedState, result)
    }

  def reportTicket(
                    carNumber: String
                  ): Reader[
    ParkingConfig,
    State[ParkingState, Writer[Log, Either[String, Double]]]
  ] =
    Reader { config =>
      State { state =>

        state.entryHours.get(carNumber) match {

          case None =>
            (state, exitError(s"Машина $carNumber не найдена для штрафа"))

          case Some(_) =>
            val fine = ticketCost.run(config)

            successfulLostTicket(carNumber, state, fine)
        }
      }
    }
}