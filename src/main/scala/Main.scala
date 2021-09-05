import java.io.FileWriter

import model.OrderType.{Buy, Sell}
import model.{ClientInfo, ClientName, ClientWithAmount, Order, OrderType, Price, State, StockName}

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.io.Source
import scala.util.Using

object Main extends App {

  val clientsInitState = readClientsInfo("clients.txt")
  val buyOrders: Map[StockName, TreeMap[Price, Vector[ClientWithAmount]]] = initOrders('A', 'D')(Ordering[Int].reverse)
  val sellOrders: Map[StockName, TreeMap[Price, Vector[ClientWithAmount]]] = initOrders('A', 'D')

  Using.resource(Source.fromFile("orders.txt")) { source =>
    val orders = source.getLines().map { str =>
      val dataArr = str.split("\t")
      Order(dataArr(0), OrderType.fromStr(dataArr(1)), dataArr(2), dataArr(3).toInt, dataArr(4).toInt)
    }
    val clientsState = processOrders(orders, clientsInitState, buyOrders, sellOrders)
    writeResult("result.txt", clientsState)
  }

  def initOrders(fromStock: Char,
                 toStock: Char)
                (implicit ord: Ordering[Int]): Map[String, TreeMap[Price, Vector[ClientWithAmount]]] =
    (fromStock to toStock).map(_.toString -> TreeMap.empty[Price, Vector[ClientWithAmount]](ord)).toMap

  @tailrec
  def processOrders(orders: Iterator[Order],
                    clientsState: Map[ClientName, ClientInfo],
                    buyOrders: Map[StockName, TreeMap[Price, Vector[ClientWithAmount]]],
                    sellOrders: Map[StockName, TreeMap[Price, Vector[ClientWithAmount]]]): Map[ClientName, ClientInfo] = {
    if (orders.hasNext) {
      val order = orders.next()
      if (isValid(order, clientsState)) {
        val updatedState = order.`type` match {
          case Sell =>
            tryMatchSell(
              order,
              clientsState,
              buyOrders.getOrElse(order.stockName, TreeMap.empty(Ordering[Int].reverse)),
              sellOrders.getOrElse(order.stockName, TreeMap.empty)
            )

          case Buy =>
            tryMatchBuy(
              order,
              clientsState,
              buyOrders.getOrElse(order.stockName, TreeMap.empty(Ordering[Int].reverse)),
              sellOrders.getOrElse(order.stockName, TreeMap.empty)
            )
        }
        processOrders(
          orders,
          updatedState.clientsState,
          buyOrders.updated(order.stockName, updatedState.buyOrders),
          sellOrders.updated(order.stockName, updatedState.sellOrders)
        )
      } else {
        processOrders(orders, clientsState, buyOrders, sellOrders)
      }
    } else {
      val balancesToReturn = buyOrders.toSeq.flatMap { case (_, orders) =>
        orders.toSeq.flatMap { case (price, clientsWithAmounts) =>
          clientsWithAmounts.map(clientWithAmount => clientWithAmount.clientName -> price * clientWithAmount.amount)
        }.groupMapReduce(_._1)(_._2)(_ + _)
      }
      val withReturnedBalances = balancesToReturn.foldLeft(clientsState) { case (res, (clientName, balanceToReturn)) =>
        res.updatedWith(clientName)(_.map(info => info.copy(balance = info.balance + balanceToReturn)))
      }
      val stocksToReturn = sellOrders.toSeq.flatMap { case (stockName, orders) =>
        orders.toSeq.flatMap { case (_, clientsWithAmounts) =>
          clientsWithAmounts.map(clientWithAmount => (clientWithAmount.clientName, stockName) -> clientWithAmount.amount)
        }
      }.groupMapReduce(_._1)(_._2)(_ + _)
      val resultClientState = stocksToReturn.foldLeft(withReturnedBalances) { case (res, ((clientName, stockName), amountToReturn)) =>
        res.updatedWith(clientName)(_.map(info => info.copy(stock = info.stock.updatedWith(stockName)(_.map(_ + amountToReturn)))))
      }
      resultClientState
    }
  }

  private def isValid(order: Order,
                      clientsState: Map[ClientName, ClientInfo]): Boolean = {
    val clientInfo = clientsState.get(order.clientName)
    clientInfo.exists { info =>
      order.`type` match {
        case Sell => info.stock.get(order.stockName).exists(_ >= order.amount)
        case Buy => info.balance >= order.price * order.amount
      }
    }
  }

  @tailrec
  private def tryMatchSell(currentOrder: Order,
                           clientsState: Map[ClientName, ClientInfo],
                           buyOrders: TreeMap[Price, Vector[ClientWithAmount]],
                           sellOrders: TreeMap[Price, Vector[ClientWithAmount]]): State =
    if (currentOrder.amount == 0) {
      State(clientsState, buyOrders, sellOrders)
    } else {
      if (buyOrders.isEmpty || currentOrder.price > buyOrders.head._1) {
        State(
          clientsState.updatedWith(currentOrder.clientName)(_.map(info => info.copy(stock = info.stock.updatedWith(currentOrder.stockName)(_.map(_ - currentOrder.amount))))),
          buyOrders,
          sellOrders.updatedWith(currentOrder.price) { q =>
            Some(q.getOrElse(Vector.empty).appended(ClientWithAmount(currentOrder.clientName, currentOrder.amount)))
          }
        )
      } else {
        val (price, orders) = buyOrders.head
        val matchedOrder = orders.head
        val matchedAmount = Math.min(currentOrder.amount, matchedOrder.amount)
        tryMatchSell(
          currentOrder.copy(amount = currentOrder.amount - matchedAmount),
          clientsState
            .updatedWith(matchedOrder.clientName)(_.map(info => info.copy(stock = info.stock.updatedWith(currentOrder.stockName)(_.map(_ + matchedAmount)))))
            .updatedWith(currentOrder.clientName)(_.map { info =>
              info.copy(
                balance = info.balance + matchedAmount * price,
                stock = info.stock.updatedWith(currentOrder.stockName)(_.map(_ - matchedAmount))
              )
            }),
          if (matchedAmount == matchedOrder.amount) {
            buyOrders.updatedWith(price) {
              case Some(Vector(_)) => None
              case Some(orders) => Some(orders.tail)
              case _ => None
            }
          } else {
            buyOrders.updatedWith(price)(_.map(_.updated(0, ClientWithAmount(matchedOrder.clientName, matchedOrder.amount - matchedAmount))))
          },
          sellOrders
        )
      }
    }

  @tailrec
  private def tryMatchBuy(currentOrder: Order,
                          clientsState: Map[ClientName, ClientInfo],
                          buyOrders: TreeMap[Price, Vector[ClientWithAmount]],
                          sellOrders: TreeMap[Price, Vector[ClientWithAmount]]): State =
    if (currentOrder.amount == 0) {
      State(clientsState, buyOrders, sellOrders)
    } else {
      if (sellOrders.isEmpty || currentOrder.price < sellOrders.head._1) {
        State(
          clientsState.updatedWith(currentOrder.clientName)(_.map(info => info.copy(balance = info.balance - currentOrder.price * currentOrder.amount))),
          buyOrders.updatedWith(currentOrder.price) { q =>
            Some(q.getOrElse(Vector.empty).appended(ClientWithAmount(currentOrder.clientName, currentOrder.amount)))
          },
          sellOrders
        )
      } else {
        val (price, orders) = sellOrders.head
        val matchedOrder = orders.head
        val matchedAmount = Math.min(currentOrder.amount, matchedOrder.amount)
        tryMatchBuy(
          currentOrder.copy(amount = currentOrder.amount - matchedAmount),
          clientsState
            .updatedWith(matchedOrder.clientName)(_.map(info => info.copy(balance = info.balance + matchedAmount * price)))
            .updatedWith(currentOrder.clientName)(_.map { info =>
              info.copy(
                balance = info.balance - matchedAmount * price,
                stock = info.stock.updatedWith(currentOrder.stockName)(_.map(_ + matchedAmount))
              )
            }),
          buyOrders,
          if (matchedAmount == matchedOrder.amount) {
            sellOrders.updatedWith(price) {
              case Some(Vector(_)) => None
              case Some(orders) => Some(orders.tail)
              case _ => None
            }
          } else {
            sellOrders.updatedWith(price)(_.map(_.updated(0, ClientWithAmount(matchedOrder.clientName, matchedOrder.amount - matchedAmount))))
          }
        )
      }
    }

  private def readClientsInfo(filename: String): Map[ClientName, ClientInfo] = {
    Using.resource(Source.fromFile(filename)) { source =>
      source.getLines().map { str =>
        val dataArr = str.split('\t')
        (
          dataArr(0),
          ClientInfo(
            dataArr(1).toInt,
            dataArr.drop(2)
              .zip('A' to 'D')
              .map { case (amountStr, stockName) =>
                stockName.toString -> amountStr.toInt
              }.toMap
          )
        )
      }.toMap
    }
  }

  private def writeResult(filename: String,
                          clientsState: Map[ClientName, ClientInfo]) = {
    Using.resource(new FileWriter(filename)) { writer =>
      clientsState.map { case (clientName, info) =>
        writer.write(
          s"$clientName\t${info.balance}\t${info.stock.getOrElse("A", 0)}\t${info.stock.getOrElse("B", 0)}\t${info.stock.getOrElse("C", 0)}\t${info.stock.getOrElse("D", 0)}\n"
        )
      }
    }
  }
}
