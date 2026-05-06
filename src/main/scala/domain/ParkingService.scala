package domain
import monads._

object ParkingLog {

  type Log = Vector[String]

  def logEnter(carNumber: String): Writer[Log, Unit] =
    Writer(Vector(s"Машина $carNumber въехала"), ())

  def logAssignedSpot(carNumber: String, spot: Int): Writer[Log, Unit] =
    Writer(Vector(s"Машине $carNumber назначено место $spot"), ())

  def logExit(carNumber: String): Writer[Log, Unit] =
    Writer(Vector(s"Машина $carNumber выехала"), ())

  def logRate(cost: Double): Writer[Log, Unit] =
    Writer(Vector(s"Начислено: $cost"), ())

  def logLostTicket(carNumber: String, fine: Double): Writer[Log, Unit] =
    Writer(Vector(s"Машина $carNumber потеряла билет. Штраф: $fine"), ())

  def logError(message: String): Writer[Log, Unit] =
    Writer(Vector(s"Ошибка: $message"), ())
}