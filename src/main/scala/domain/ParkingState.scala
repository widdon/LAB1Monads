package domain

final case class ParkingState(
                               occupiedSpots: Set[Int], // Какие места заняты
                               entryHours: Map[String, Int], // Время въезда определенной машины
                               spotByCar: Map[String, Int], // Какое место занимает определенная машина
                               currentHour: Int, // Какое сейчас время
                               profit: Double // Сколько заработала парковка
                             )

object ParkingState {

  // Нулевое состояние
  def empty: ParkingState =
    ParkingState(
      occupiedSpots = Set.empty,
      entryHours = Map.empty,
      spotByCar = Map.empty,
      currentHour = 0,
      profit = 0.0
    )
}
