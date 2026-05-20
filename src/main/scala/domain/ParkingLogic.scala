package domain

import monads._
import ParkingService._
import ParkingConfigService._

object ParkingLogic {

  private def findFree(
                        occupied: Set[Int],
                        capacity: Int
                      ): Option[Int] =
    (1 to capacity).find(spot => !occupied.contains(spot))

  private def enterError(message: String): Writer[Log, Either[String, Int]] =
    for {
      _ <- logError(message)
    } yield Left(message): Either[String, Int]

  private def exitError(message: String): Writer[Log, Either[String, Double]] =
    for {
      _ <- logError(message)
    } yield Left(message): Either[String, Double]

  private def alreadyInside(carNumber: String, state: ParkingState): Option[String] =
    Option.when(state.entryHours.contains(carNumber)) {s"Машина $carNumber уже находится на парковке"}

  private def noFreePlaces(freePlaces: Int): Option[String] =
    Option.when(freePlaces <= 0) {"Нет свободных мест"}

  private def validateEnter(
                             carNumber: String,
                             state: ParkingState,
                             capacity: Int
                           ): Either[String, Int] = {

    val freePlaces = capacity - state.occupiedSpots.size

    for {

      _ <- Either.cond(!state.entryHours.contains(carNumber), (), s"Машина $carNumber уже находится на парковке")

      _ <- Either.cond(freePlaces > 0, (), "Нет свободных мест")

      spot <- findFree(state.occupiedSpots, capacity).toRight("Свободное место не найдено")

    } yield spot
  }

  private def findEntryTime(
                             carNumber: String,
                             state: ParkingState
                           ): Either[String, Int] =
    state.entryHours
      .get(carNumber)
      .toRight(s"Машина $carNumber не найдена")

  private def validateTicket(
                              carNumber: String,
                              state: ParkingState
                            ): Either[String, Unit] =
    Either.cond(state.entryHours.contains(carNumber), (), s"Машина $carNumber не найдена для штрафа")

  private def successfulEnter(
                               carNumber: String,
                               state: ParkingState,
                               spot: Int
                             ): (
    ParkingState, Writer[Log, Either[String, Int]]) = {

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
                            ): (
    ParkingState, Writer[Log, Either[String, Double]]) = {

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
                                  ): (
    ParkingState, Writer[Log, Either[String, Double]]) = {

    val spot =
      state.spotByCar(carNumber)

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

  def enterCar(carNumber: String): Reader[
    ParkingConfig, State[ParkingState, Writer[Log, Either[String, Int]]]] =
    Reader { config =>
      State { state =>

        validateEnter(carNumber, state, config.capacity)
        match {

          case Right(spot) => successfulEnter(carNumber, state, spot)

          case Left(error) => (state, enterError(error))
        }
      }
    }

  def exitCar(carNumber: String): Reader[
    ParkingConfig, State[ParkingState, Writer[Log, Either[String, Double]]]] =
    Reader { config =>
      State { state =>

        findEntryTime(carNumber, state)
        match {

          case Left(error) => (state, exitError(error))

          case Right(entryTime) =>

            val cost = totalCost(entryTime, state.currentHour).run(config)

            successfulExit(carNumber, state, cost)
        }
      }
    }

  def nextHour:
  State[ParkingState, Writer[Log, Int]] =
    State { state =>

      val newHour = state.currentHour + 1

      val updatedState = state.copy(currentHour = newHour)

      val result =
        Writer(
          Vector(s"Время переведено. Текущий час: $newHour"),
          newHour
        )

      (updatedState, result)
    }

  def reportTicket(carNumber: String): Reader[
    ParkingConfig, State[ParkingState, Writer[Log, Either[String, Double]]]] =
    Reader { config =>
      State { state =>

        validateTicket(carNumber, state)
        match {

          case Left(error) =>
            (
              state,
              exitError(error)
            )

          case Right(_) =>

            val fine =
              ticketCost.run(config)

            successfulLostTicket(carNumber, state, fine)
        }
      }
    }
}