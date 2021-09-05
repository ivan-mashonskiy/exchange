import model.{ClientInfo, ClientName, Order}
import model.OrderType.{Buy, Sell}
import ProcessOrdersSpec._
import Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProcessOrdersSpec extends AnyFlatSpec with Matchers {

  "processOrders" should "match entire orders" in {
    withInitializedOrders(FullMatchOrders.iterator, Clients) shouldBe FullMatchClients
  }

  it should "not match orders" in {
    withInitializedOrders(NotMatchOrders.iterator, Clients) shouldBe Clients
  }

  it should "ignore not correct sell orders" in {
    withInitializedOrders(NotCorrectSellOrders.iterator, Clients) shouldBe Clients
  }

  it should "ignore not correct buy orders" in {
    withInitializedOrders(NotCorrectBuyOrders.iterator, Clients) shouldBe Clients
  }

  it should "partially match orders" in {
    withInitializedOrders(PartialMatchOrders.iterator, Clients) shouldBe PartialMatchClients
  }

  it should "match orders with the same price in chronological order" in {
    withInitializedOrders(ChronologicalMatchOrders.iterator, Clients) shouldBe ChronologicalMatchClients
  }

  private def withInitializedOrders(orders: Iterator[Order],
                                    clientsState: Map[ClientName, ClientInfo]): Map[ClientName, ClientInfo] =
    processOrders(orders, clientsState, initOrders('A', 'D')(Ordering[Int].reverse), initOrders('A', 'D'))
}

object ProcessOrdersSpec {
  val Clients: Map[ClientName, ClientInfo] = Map(
    "C1" -> ClientInfo(1000, Map("A" -> 10, "B" -> 5, "C" -> 15, "D" -> 0)),
    "C2" -> ClientInfo(2000, Map("A" -> 3, "B" -> 35, "C" -> 40, "D" -> 10)),
    "C3" -> ClientInfo(3000, Map("A" -> 20, "B" -> 30, "C" -> 40, "D" -> 50))
  )

  val FullMatchOrders: Seq[Order] = Seq(
    Order("C1", Buy, "A", 10, 3),
    Order("C2", Sell, "A", 10, 3)
  )
  val FullMatchClients: Map[ClientName, ClientInfo] = Map(
    "C1" -> ClientInfo(970, Map("A" -> 13, "B" -> 5, "C" -> 15, "D" -> 0)),
    "C2" -> ClientInfo(2030, Map("A" -> 0, "B" -> 35, "C" -> 40, "D" -> 10)),
    "C3" -> ClientInfo(3000, Map("A" -> 20, "B" -> 30, "C" -> 40, "D" -> 50))
  )

  val NotMatchOrders: Seq[Order] = Seq(
    Order("C1", Buy, "A", 10, 3),
    Order("C2", Sell, "A", 12, 3)
  )

  val NotCorrectSellOrders: Seq[Order] = Seq(
    Order("C1", Buy, "A", 10, 3),
    Order("C2", Sell, "A", 10, 30)
  )

  val NotCorrectBuyOrders: Seq[Order] = Seq(
    Order("C1", Buy, "A", 1000, 3),
    Order("C2", Sell, "A", 1000, 3)
  )

  val PartialMatchOrders: Seq[Order] = Seq(
    Order("C1", Buy, "A", 10, 12),
    Order("C2", Sell, "A", 10, 3)
  )
  val PartialMatchClients: Map[ClientName, ClientInfo] = Map(
    "C1" -> ClientInfo(970, Map("A" -> 13, "B" -> 5, "C" -> 15, "D" -> 0)),
    "C2" -> ClientInfo(2030, Map("A" -> 0, "B" -> 35, "C" -> 40, "D" -> 10)),
    "C3" -> ClientInfo(3000, Map("A" -> 20, "B" -> 30, "C" -> 40, "D" -> 50))
  )

  val ChronologicalMatchOrders: Seq[Order] = Seq(
    Order("C1", Buy, "A", 10, 10),
    Order("C2", Buy, "A", 10, 10),
    Order("C3", Sell, "A", 10, 15),
  )
  val ChronologicalMatchClients: Map[ClientName, ClientInfo] = Map(
    "C1" -> ClientInfo(900, Map("A" -> 20, "B" -> 5, "C" -> 15, "D" -> 0)),
    "C2" -> ClientInfo(1950, Map("A" -> 8, "B" -> 35, "C" -> 40, "D" -> 10)),
    "C3" -> ClientInfo(3150, Map("A" -> 5, "B" -> 30, "C" -> 40, "D" -> 50))
  )
}
