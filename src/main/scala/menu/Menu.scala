package menu

import domain._
import domain.ParkingLogic._
import monads._

// ======================================================
// Лист меню — конкретное действие
// ======================================================
final case class MenuLeaf(
                           title: String,
                           action: (ParkingState, ParkingConfig) => IO[ParkingState]
                         ) extends MenuOption {

  override def execute(
                        state: ParkingState,
                        config: ParkingConfig
                      ): IO[ParkingState] =
    action(state, config)
}

// ======================================================
// Узел дерева — меню / подменю
// ======================================================
final case class MenuTreeNode(
                               title: String,
                               options: Seq[MenuOption]
                             ) extends MenuOption
  with UserInteraction {

  override def execute(
                        state: ParkingState,
                        config: ParkingConfig
                      ): IO[ParkingState] =
    userInteractionLoop(state, config)

  // ----------------------------------------------------
  // Отображение меню
  // ----------------------------------------------------
  override def show(state: ParkingState): String = {

    val items =
      options.zipWithIndex
        .map { case (option, index) =>
          s"${index + 1}  ${option.title}"
        }
        .mkString("\n")

    s"""
       |--- $title ---
       |Текущий час: ${state.currentHour}
       |Занято мест: ${state.occupiedSpots.size}
       |Выручка: ${state.profit}
       |
       |$items
       |0  выход
       |выбор:
       |""".stripMargin
  }

  // ----------------------------------------------------
  // Обработка выбора
  // ----------------------------------------------------
  override def handleInput(
                            input: String,
                            state: ParkingState,
                            config: ParkingConfig
                          ): IO[Option[ParkingState]] = {

    input.trim.toIntOption match {

      case Some(0) =>
        IO.pure(None)

      case Some(index) if index >= 1 && index <= options.size =>
        options(index - 1)
          .execute(state, config)
          .map(newState => Some(newState))

      case _ =>
        IO.printLine("Неизвестная команда")
          .map(_ => Some(state))
    }
  }
}