package menu

import domain.{ParkingConfig, ParkingState}
import monads.IO

// Универсальный цикл взаимодействия с пользователем
trait UserInteraction {

  // Показать меню
  def show(state: ParkingState): String

  // Чтение ввода
  def readInput: IO[String] =
    IO.readLine

  // Обработка команды
  def handleInput(
                   input: String,
                   state: ParkingState,
                   config: ParkingConfig
                 ): IO[Option[ParkingState]]

  // Основной цикл
  def userInteractionLoop(
                           state: ParkingState,
                           config: ParkingConfig
                         ): IO[ParkingState] =
    for {
      _ <- IO.printLine(show(state))
      input <- readInput
      next <- handleInput(input, state, config)

      result <- next match {
        case Some(newState) =>
          userInteractionLoop(newState, config)

        case None =>
          IO.pure(state)
      }

    } yield result
}