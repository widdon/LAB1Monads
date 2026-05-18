package menu

import domain.{ParkingConfig, ParkingState}
import monads.IO

// Общий пункт меню
trait MenuOption {

  def title: String

  def execute(
               state: ParkingState,
               config: ParkingConfig
             ): IO[ParkingState]
}