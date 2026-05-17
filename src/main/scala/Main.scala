// ======================================================
// FILE: Main.scala
// package app
// ======================================================

import monads._
import domain._
import domain.ParkingLogic._

object Main {

  // Конфиг для настройки парковки
  val config: ParkingConfig =
    ParkingConfig(
      capacity = 10,
      hourlyRate = 100.0,
      lostTicketFine = 5000.0,
      roundUpToHour = true
    )

  // Печать логов через Writer
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
                           carNumber: String,
                           state: ParkingState
                         ): IO[ParkingState] = {

    val stateProgram = enterCar(carNumber).run(config)
    val (newState, writerResult) = stateProgram.run(state)

    writerResult.value match {
      case Right(spot) =>
        for {
          _ <- printLogs(writerResult.log)
          _ <- IO.printLine(s"Машина $carNumber припаркована на месте: $spot")
        } yield newState

      case Left(error) =>
        for {
          _ <- printLogs(writerResult.log)
          _ <- IO.printLine(s"Ошибка: $error")
        } yield newState
    }
  }
  

  private def handleExit(
                          carNumber: String,
                          state: ParkingState
                        ): IO[ParkingState] = {

    val stateProgram = exitCar(carNumber).run(config)
    val (newState, writerResult) = stateProgram.run(state)

    writerResult.value match {
      case Right(cost) =>
        for {
          _ <- printLogs(writerResult.log)
          _ <- IO.printLine("===== ЧЕК =====")
          _ <- IO.printLine(s"Машина: $carNumber")
          _ <- IO.printLine(s"Стоимость: $cost")
          _ <- IO.printLine("================")
        } yield newState

      case Left(error) =>
        for {
          _ <- printLogs(writerResult.log)
          _ <- IO.printLine(s"Ошибка: $error")
        } yield newState
    }
  }
  
  // Команда lost
  private def handleLostTicket(
                                carNumber: String,
                                state: ParkingState
                              ): IO[ParkingState] = {

    val stateProgram = reportTicket(carNumber).run(config)
    val (newState, writerResult) = stateProgram.run(state)

    writerResult.value match {
      case Right(fine) =>
        for {
          _ <- printLogs(writerResult.log)
          _ <- IO.printLine(s"Штраф за потерянный билет: $fine")
        } yield newState

      case Left(error) =>
        for {
          _ <- printLogs(writerResult.log)
          _ <- IO.printLine(s"Ошибка: $error")
        } yield newState
    }
  }
  
  private def handleNextHour(
                              state: ParkingState
                            ): IO[ParkingState] = {

    val (newState, writerResult) = nextHour.run(state)

    for {
      _ <- printLogs(writerResult.log)
    } yield newState
  }
  
  private def menu: IO[Unit] =
    for {
      _ <- IO.printLine("")
      _ <- IO.printLine("Выберите действие:")
      _ <- IO.printLine("enter - въезд")
      _ <- IO.printLine("exit  - выезд")
      _ <- IO.printLine("lost  - потерян билет")
      _ <- IO.printLine("next  - следующий час")
      _ <- IO.printLine("state - состояние парковки")
      _ <- IO.printLine("quit  - выход")
    } yield ()
  
  def loop(state: ParkingState): IO[Unit] =
    for {
      _ <- menu
      _ <- IO.printLine("Введите команду:")
      command <- IO.readLine.map(_.trim.toLowerCase)

      _ <- command match {

        case "enter" =>
          for {
            _ <- IO.printLine("Введите номер машины:")
            carNumber <- IO.readLine
            newState <- handleEnter(carNumber, state)
            _ <- loop(newState)
          } yield ()

        case "exit" =>
          for {
            _ <- IO.printLine("Введите номер машины:")
            carNumber <- IO.readLine
            newState <- handleExit(carNumber, state)
            _ <- loop(newState)
          } yield ()

        case "lost" =>
          for {
            _ <- IO.printLine("Введите номер машины:")
            carNumber <- IO.readLine
            newState <- handleLostTicket(carNumber, state)
            _ <- loop(newState)
          } yield ()

        case "next" =>
          for {
            newState <- handleNextHour(state)
            _ <- loop(newState)
          } yield ()

        case "state" =>
          for {
            _ <- printState(state)
            _ <- loop(state)
          } yield ()

        case "quit" =>
          IO.printLine("Завершение работы...")

        case _ =>
          for {
            _ <- IO.printLine("Неизвестная команда")
            _ <- loop(state)
          } yield ()
      }

    } yield ()

  
  def main(args: Array[String]): Unit = {

    val program =
      for {
        _ <- IO.printLine("=== СИСТЕМА УПРАВЛЕНИЯ ПАРКОВКОЙ ===")
        _ <- loop(ParkingState.empty)
      } yield ()

    program.unsafeRun()
  }
}