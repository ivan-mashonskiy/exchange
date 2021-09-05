package model

case class ClientInfo(balance: Int,
                      stock: Map[StockName, Int])
