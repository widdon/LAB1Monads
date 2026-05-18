import monads._
import domain._
import domain.ParkingLogic._
import menu._

object Main {

  // Конфиг для настройки парковки
  val config: ParkingConfig =
    ParkingConfig(
      capacity = 10,
      hourlyRate = 100.0,
      lostTicketFine = 5000.0,
      roundUpToHour = true
    )

  // Печать логов
  private def printLogs(logs: Vector[String]): IO[Unit] = {
    val actions = logs.map(line => IO.printLine(s"[LOG] $line"))
    IO.sequence(actions).map(_ => ())
  }


  private def printState(state: ParkingState): IO[Unit] =
    for {
      _ <- IO.printLine("===== СОСТОЯНИЕ ПАРКОВКИ =====")
      _ <- IO.printLine(s"Текущий час: ${state.currentHour}")
      _ <- IO.printLine(s"Занято мест: ${state.occupiedSpots.size}")
      _ <- IO.printLine(s"Свободно мест: ${config.capacity - state.occupiedSpots.size}")
      _ <- IO.printLine(s"Выручка: ${state.profit}")
      _ <- IO.printLine("==============================")
    } yield ()


  private def handleEnter(
                           state: ParkingState,
                           config: ParkingConfig
                         ): IO[ParkingState] =
    for {
      _ <- IO.printLine("Введите номер машины:")
      carNumber <- IO.readLine

      stateProgram = enterCar(carNumber).run(config)
      resultTuple = stateProgram.run(state)

      (newState, writerResult) = resultTuple

      _ <- printLogs(writerResult.log)

      _ <- writerResult.value match {
        case Right(spot) =>
          IO.printLine(s"Машина припаркована на месте: $spot")

        case Left(error) =>
          IO.printLine(s"Ошибка: $error")
      }

    } yield newState


  private def handleExit(
                          state: ParkingState,
                          config: ParkingConfig
                        ): IO[ParkingState] =
    for {
      _ <- IO.printLine("Введите номер машины:")
      carNumber <- IO.readLine

      stateProgram = exitCar(carNumber).run(config)
      resultTuple = stateProgram.run(state)

      (newState, writerResult) = resultTuple

      _ <- printLogs(writerResult.log)

      _ <- writerResult.value match {
        case Right(cost) =>
          for {
            _ <- IO.printLine("===== ЧЕК =====")
            _ <- IO.printLine(s"Машина: $carNumber")
            _ <- IO.printLine(s"Стоимость: $cost")
          } yield ()

        case Left(error) =>
          IO.printLine(s"Ошибка: $error")
      }

    } yield newState

  // Команда lost
  private def handleLostTicket(
                                state: ParkingState,
                                config: ParkingConfig
                              ): IO[ParkingState] =
    for {
      _ <- IO.printLine("Введите номер машины:")
      carNumber <- IO.readLine

      stateProgram = reportTicket(carNumber).run(config)
      resultTuple = stateProgram.run(state)

      (newState, writerResult) = resultTuple

      _ <- printLogs(writerResult.log)

      _ <- writerResult.value match {
        case Right(fine) =>
          IO.printLine(s"Штраф: $fine")

        case Left(error) =>
          IO.printLine(s"Ошибка: $error")
      }

    } yield newState

  private def handleNextHour(
                              state: ParkingState,
                              config: ParkingConfig
                            ): IO[ParkingState] = {

    val (newState, writerResult) = nextHour.run(state)

    for {
      _ <- printLogs(writerResult.log)
    } yield newState
  }

  val mainMenu: MenuTreeNode =
    MenuTreeNode(
      title = "СИСТЕМА УПРАВЛЕНИЯ ПАРКОВКОЙ",
      options = Seq(
        MenuLeaf("Въезд машины", handleEnter),
        MenuLeaf("Выезд машины", handleExit),
        MenuLeaf("Потерян билет", handleLostTicket),
        MenuLeaf("Следующий час", handleNextHour)
      )
    )


  def main(args: Array[String]): Unit = {

    val program =
      for {
        finalState <- mainMenu.execute(
          ParkingState.empty,
          config
        )

        _ <- IO.printLine(
          s"Работа завершена. Итоговая выручка: ${finalState.profit}"
        )

      } yield ()

    program.unsafeRun()
  }
}