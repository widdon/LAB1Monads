package domain

sealed trait ParkingResult

object ParkingResult {

  final case class EnterSuccess(spot: Int) extends ParkingResult

  final case class ExitSuccess(cost: Double) extends ParkingResult

  final case class LostTicketSuccess(fine: Double) extends ParkingResult

  final case class Failure(reason: String) extends ParkingResult
}
