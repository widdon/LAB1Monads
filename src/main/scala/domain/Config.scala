package domain
import monads._

final case class ParkingConfig(
                                capacity: Int,          // Число мест на парковке
                                hourlyRate: Double,     // Тариф за час
                                lostTicketFine: Double, // Штраф за потерянный билет
                                roundUpToHour: Boolean  // Округлять ли вверх
                              )

object ParkingConfigService {

  // Сможет ли машина вьехать?
  def canEnter(freePlaces: Int): Reader[ParkingConfig, Boolean] =
    Reader(_ => freePlaces > 0)

  // Округление времени по правилу из конфига
  private def roundingHours(hours: Double): Reader[ParkingConfig, Double] =
    Reader { config =>
      if (config.roundUpToHour) math.ceil(hours)
      else hours
    }

  // Стоимость парковки по количеству часов
  def timeToCost(hours: Double): Reader[ParkingConfig, Double] =
    for {
      normalized <- roundingHours(hours)
      config <- Reader.ask[ParkingConfig]
    } yield normalized * config.hourlyRate

  // Стоимость потерянного билета
  def ticketCost: Reader[ParkingConfig, Double] =
    Reader(_.lostTicketFine)

  // Общий расчет стоимости парковки 
  def totalCost(entryTime: Int, exitTime: Int): Reader[ParkingConfig, Double] = {
    val hours = (exitTime - entryTime).toDouble
    timeToCost(hours)
  }
}