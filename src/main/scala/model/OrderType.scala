package model

sealed trait OrderType

object OrderType {
  case object Sell extends OrderType
  case object Buy extends OrderType

  def fromStr(str: String): OrderType =
    str match {
      case "s" => Sell
      case _ => Buy
    }
}