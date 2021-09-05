package model

case class Order(clientName: ClientName,
                 `type`: OrderType,
                 stockName: StockName,
                 price: Price,
                 amount: Int)
