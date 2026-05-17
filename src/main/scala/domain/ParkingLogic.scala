package domain
import monads._

object ParkingLogic {

  import ParkingService._
  import ParkingConfigService._


  private def findFree(occupied: Set[Int], capacity: Int): Option[Int] =
    (1 to capacity).find(spot => !occupied.contains(spot))


  def enterCar(carNumber: String): Reader[ParkingConfig, State[ParkingState, Writer[Log, Either[String, Int]]]] =
    Reader { config =>
      State { state =>

        val freePlaces = config.capacity - state.occupiedSpots.size

        if (state.entryHours.contains(carNumber)) {
          val result =
            for {
              _ <- logError(s"Машина $carNumber уже находится на парковке")
            } yield (Left("Машина уже на парковке"): Either[String, Int])

          (state, result)

        } else if (freePlaces <= 0) {
          val result =
            for {
              _ <- logError("Нет свободных мест")
            } yield (Left("Парковка полная"): Either[String, Int])
          (state, result)

        } else {
          val spot = findFree(state.occupiedSpots, config.capacity).get

          val updatedState = state.copy(
            occupiedSpots = state.occupiedSpots + spot,
            entryHours = state.entryHours + (carNumber -> state.currentHour),
            spotByCar = state.spotByCar + (carNumber -> spot)
          )

          val result =
            for {
              _ <- logEnter(carNumber)
              _ <- logAssignedSpot(carNumber, spot)
            } yield (Right(spot): Either[String, Int])

          (updatedState, result)
        }
      }
    }


  def exitCar(carNumber: String): Reader[ParkingConfig, State[ParkingState, Writer[Log, Either[String, Double]]]] =
    Reader { config =>
      State { state =>

        state.entryHours.get(carNumber) match {

          case None =>
            val result =
              for {
                _ <- logError(s"Машина $carNumber не найдена")
              } yield (Left("Машина не найдена"): Either[String, Double])

            (state, result)

          case Some(entryTime) =>
            val exitTime = state.currentHour
            val cost = totalCost(entryTime, exitTime).run(config)
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
              } yield (Right(cost): Either[String, Double])

            (updatedState, result)
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


  def reportTicket(carNumber: String): Reader[ParkingConfig, State[ParkingState, Writer[Log, Either[String, Double]]]] =
    Reader { config =>
      State { state =>

        state.entryHours.get(carNumber) match {

          case None =>
            val result =
              for {
                _ <- logError(s"Машина $carNumber не найдена для штрафа")
              } yield (Left("Машина не найдена"): Either[String, Double])

            (state, result)

          case Some(_) =>
            val fine = ticketCost.run(config)
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
              } yield (Right(fine): Either[String, Double])

            (updatedState, result)
        }
      }
    }
}